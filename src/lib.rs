
use proc_macro::TokenStream;
use proc_macro2::{
	TokenStream as TokenStream2,
};
use quote::{
	ToTokens,
	quote,
	format_ident,
};
use std::{
	collections::HashMap,
	mem::{
		align_of,
		size_of,
	},
};
use syn::{
	Data,
	DeriveInput,
	Error as SynError,
	Expr,
	Fields,
	Ident,
	Lit,
	Meta,
	MetaNameValue,
	Token,
	Type,
	parse_macro_input,
	parse_quote,
	punctuated::Punctuated,
	spanned::Spanned,
};

#[proc_macro_attribute]
pub fn ffi_struct(_attr: TokenStream, item: TokenStream) -> TokenStream {
	let input = parse_macro_input!(item as DeriveInput);
	let output = match impl_ffi_struct(input) {
		Ok(ts) => ts,
		Err(e) => e.to_compile_error().into(),
	};
	TokenStream::from(output)
}

fn impl_ffi_struct(mut input: DeriveInput) -> Result<TokenStream2, SynError> {
	// Verify input is a struct
	let data = match &mut input.data {
		Data::Struct(s) => s,
		_ => return Err(SynError::new(input.span(), "ffi_struct can only be applied to structs")),
	};

	// Validate struct name ends with Rust
	let ident = &input.ident;
	let ident_str = ident.to_string();
	if !ident_str.ends_with("Rust") {
		return Err(SynError::new(ident.span(), "Struct name must end with 'Rust'"));
	}
	let new_ident_str = ident_str.trim_end_matches("Rust");
	let new_ident = Ident::new(new_ident_str, ident.span());

	// Extract generics and where clause from the original struct
	let generics = &input.generics;
	let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

	let mut size_of_types = HashMap::new();
	for attr in &input.attrs {
		if attr.path().is_ident("size_of_type") {
			if let Meta::List(list) = &attr.meta {
				// Parse comma-separated list of name-value pairs
				let parsed = list.parse_args_with(Punctuated::<MetaNameValue, Token![,]>::parse_terminated)?;

				for nv in parsed {
					if let Some(ident) = nv.path.get_ident() {
						if let Expr::Lit(expr_lit) = &nv.value {
							if let Lit::Int(lit_int) = &expr_lit.lit {
								let size = lit_int.base10_parse::<usize>()?;
								size_of_types.insert(ident.to_string(), size);
							}
						}
					}
				}
			}
		}
	}

	// Preserve all non-macro attributes from the original struct, but remove #[derive(Default)]
	let mut preserved_attrs = Vec::new();
	for attr in &input.attrs {
		if !attr.path().is_ident("size_of_type") && !attr.path().is_ident("ffi_struct") {
			if let Meta::List(list) = &attr.meta {
				if list.path.is_ident("derive") {
					// Parse the derive list
					if let Ok(derives) = list.parse_args_with(Punctuated::<Path, Token![,]>::parse_terminated) {
						// Create a new list without Default
						let mut new_derives: Punctuated<syn::Path, Token![,]> = Punctuated::new();
						for path in derives {
							if !path.is_ident("Default") {
								new_derives.push(path);
							}
						}

						// If there are still derives left, add them back
						if !new_derives.is_empty() {
							preserved_attrs.push(parse_quote! {
								#[derive(#new_derives)]
							});
						}
					} else {
						preserved_attrs.push(attr.clone());
					}
				} else {
					preserved_attrs.push(attr.clone());
				}
			} else {
				preserved_attrs.push(attr.clone());
			}
		}
	}

	// Extract named fields
	let fields = match &mut data.fields {
		Fields::Named(fields) => &mut fields.named,
		_ => return Err(SynError::new(input.span(), "ffi_struct only supports structs with named fields")),
	};

	// Parse field attributes and remove macro-specific ones
	let mut field_infos = Vec::new();
	for field in fields.iter_mut() {
		let name = field.ident.as_ref().ok_or_else(|| syn::Error::new(field.span(), "Field must have a name"))?;
		let ty = &field.ty;
		let is_generic = is_generic_type(ty);

		// Parse attributes and remove macro-specific ones
		let mut align_attr: Option<usize> = None;
		let mut size_attr: Option<usize> = None;

		// Remove #[align] and #[size] attributes from field
		field.attrs.retain(|attr| {
			if attr.path().is_ident("align") {
				if let Meta::NameValue(nv) = &attr.meta {
					if let Expr::Lit(expr_lit) = &nv.value {
						if let Lit::Int(lit) = &expr_lit.lit {
							align_attr = Some(lit.base10_parse().unwrap());
						}
					}
				}
				false // Remove this attribute
			} else if attr.path().is_ident("size") {
				if let Meta::NameValue(nv) = &attr.meta {
					if let Expr::Lit(expr_lit) = &nv.value {
						if let Lit::Int(lit) = &expr_lit.lit {
							size_attr = Some(lit.base10_parse().unwrap());
						}
					}
				}
				false // Remove this attribute
			} else {
				true // Keep other attributes
			}
		});

		// Default alignment if not provided
		let align_attr = align_attr.unwrap_or(1);

		// Calculate size for non-generic fields if not provided
		let size_attr = if !is_generic && size_attr.is_none() {
			Some(calculate_type_size(ty, &size_of_types)?)
		} else {
			size_attr
		};

		// Generic fields must have size attribute
		if is_generic && size_attr.is_none() {
			return Err(syn::Error::new(field.span(), "Generic field must have #[size(Xxx)] attribute"));
		}

		field_infos.push((name.clone(), ty.clone(), align_attr, size_attr.unwrap(), is_generic));
	}

	// Generate new struct fields with padding
	let mut new_fields = Vec::new();
	let mut pad_count: usize = 0;
	let mut offset: usize = 0;
	let mut field_entries = Vec::new();

	for (name, ty, align, size, is_generic) in field_infos.iter() {
		// Calculate padding needed for alignment
		let padding = if offset % align == 0 { 0 } else { align - (offset % align) };

		// Insert padding field if needed
		if padding > 0 {
			let pad_name = format_ident!("_pad{}", pad_count);
			new_fields.push(quote! {
				pub #pad_name: [u8; #padding]
			});
			field_entries.push((pad_name.clone(), None, padding, *align, false));
			pad_count += 1;
			offset += padding;
		}

		// Add original field
		new_fields.push(quote! {
			#name: #ty
		});

		field_entries.push((name.clone(), Some(ty.clone()), *size, *align, *is_generic));
		offset += size;
	}

	// Get visibility of original struct
	let vis = &input.vis;

	// Generate new struct definition
	let new_struct = quote! {
		#(#preserved_attrs)*
		#[repr(C)]
		#vis struct #new_ident {
			#(#new_fields,)*
		}
	};

	// Generate From/Into implementations
	let from_impl = {
		let mut assignments_to_new = Vec::new();
		let mut assignments_to_old = Vec::new();

		for (name, _, _, _, _) in &field_infos {
			assignments_to_new.push(quote! {
				new.#name = value.#name;
			});
			assignments_to_old.push(quote! {
				old.#name = value.#name;
			});
		}

		quote! {
			impl #ident {
				pub fn into_ffi(self) -> #new_ident {
					self.into()
				}
			}

			impl #new_ident {
				pub fn into_rust(self) -> #ident {
					self.into()
				}
			}

			impl From<#ident> for #new_ident {
				fn from(value: #ident) -> Self {
					let mut new = #new_ident::default();
					#(#assignments_to_new)*
					new
				}
			}

			impl From<#new_ident> for #ident {
				fn from(value: #new_ident) -> Self {
					let mut old = #ident::default();
					#(#assignments_to_old)*
					old
				}
			}
		}
	};

	// Generate Default implementation for new struct
	let default_impl = {
		let mut field_inits = Vec::new();
		for (name, _, _, _, _) in &field_infos {
			field_inits.push(quote! {
				#name: Default::default()
			});
		}

		// Also include padding fields
		for i in 0..pad_count {
			let pad_name = format_ident!("_pad{}", i);
			field_inits.push(quote! {
				#pad_name: Default::default()
			});
		}

		quote! {
			impl Default for #new_ident {
				fn default() -> Self {
					#new_ident {
						#(#field_inits,)*
					}
				}
			}
		}
	};

	// Implement FFIStruct trait
	let trait_impl = {
		let mut field_computations = Vec::new();
		for (name, ty, size, _align, is_generic) in field_entries.iter() {
			let is_pad = name.to_string().starts_with("_pad");
			let field_name = name.to_string();

			let field_ty = if is_pad {
				quote! { [u8] }
			} else {
				quote! { #ty }
			};

			// Get the type name string
			let type_name_str = if is_pad {
				format!("[u8; {}]", size)
			} else {
				// Convert type name to string
				ty.to_token_stream().to_string()
			};

			field_computations.push(quote! {
				{
					// Safe field offset calculation
					let base_ptr = self as *const Self as *const u8 as usize;

					// Correct pointer casting
					let field_ptr = (&self.#name) as *const #field_ty as *const u8 as usize;

					// Calculate offset using simple arithmetic
					let offset = field_ptr - base_ptr;

					// Runtime size validation for generic fields
					if !#is_pad && #is_generic {
						let actual_size = std::mem::size_of_val(&self.#name);
						if actual_size != #size {
							panic!(
								"Runtime size check failed for field {}: expected {}, actual {}. Please update #[size({})] and recompile.",
								#field_name, #size, actual_size, actual_size
							);
						}
					}

					// Determine actual size
					let size = if #is_pad {
						#size
					} else {
						std::mem::size_of_val(&self.#name)
					};

					// Get type ID using type_id() method
					let type_id = if #is_pad {
						std::any::TypeId::of::<[u8]>()
					} else {
						self.#name.type_id()
					};

					// 添加类型名称
					let type_name = #type_name_str;

					(stringify!(#name), MemberInfo { size, type_id, offset, type_name })
				}
			});
		}

		quote! {
			impl FFIStruct for #new_ident {
				type RustType = #ident;

				fn iter_members(&self) -> std::vec::IntoIter<(&'static str, MemberInfo)> {
					let mut members = Vec::new();
					#(
						let member = #field_computations;
						if !member.0.starts_with("_pad") {
							members.push(member);
						}
					)*
					members.into_iter()
				}

				fn iter_all_members(&self) -> std::vec::IntoIter<(&'static str, MemberInfo)> {
					let mut members = Vec::new();
					#(
						members.push(#field_computations);
					)*
					members.into_iter()
				}
			}
		}
	};

	// Combine all generated code
	let output = quote! {
		#input

		#new_struct

		#from_impl
		#default_impl

		#trait_impl
	};

	Ok(output)
}

// Check if type contains generic parameters
fn is_generic_type(ty: &Type) -> bool {
	match ty {
		Type::Path(type_path) => type_path.path.segments.iter().any(|seg| !seg.arguments.is_empty()),
		_ => false,
	}
}

// Calculate size of complex types at compile time
fn calculate_type_size(ty: &Type, size_of_types: &HashMap<String, usize>) -> Result<usize, SynError> {
	match ty {
		// Basic types
		Type::Path(type_path) => {
			if let Some(ident) = type_path.path.get_ident() {
				let type_name = ident.to_string();

				// Check if size is provided via #[size_of_type]
				if let Some(size) = size_of_types.get(&type_name) {
					return Ok(*size);
				}

				// Handle basic types
				match type_name.as_str() {
					"i8" =>    Ok(size_of::<i8>()),
					"u8" =>    Ok(size_of::<u8>()),
					"i16" =>   Ok(size_of::<i16>()),
					"u16" =>   Ok(size_of::<u16>()),
					"i32" =>   Ok(size_of::<i32>()),
					"u32" =>   Ok(size_of::<u32>()),
					"i64" =>   Ok(size_of::<i64>()),
					"u64" =>   Ok(size_of::<u64>()),
					"f32" =>   Ok(size_of::<f32>()),
					"f64" =>   Ok(size_of::<f64>()),
					"bool" =>  Ok(size_of::<bool>()),
					"char" =>  Ok(size_of::<char>()),
					"isize" => Ok(size_of::<isize>()),
					"usize" => Ok(size_of::<usize>()),
					_ => Err(SynError::new(
						ident.span(),
						format!("Cannot determine size of type '{}'. Please use #[size] attribute", type_name)
					)),
				}
			} else {
				Err(SynError::new(
					ty.span(),
					"Cannot determine size of qualified type. Please use #[size] attribute"
				))
			}
		},

		// Arrays: [T; N]
		Type::Array(array) => {
			let elem_size = calculate_type_size(&array.elem, size_of_types)?;
			if let Expr::Lit(lit) = &array.len {
				if let Lit::Int(int_lit) = &lit.lit {
					let len: usize = int_lit.base10_parse()?;
					return Ok(elem_size * len);
				}
			}
			Err(SynError::new(array.len.span(), "Array length must be a constant integer"))
		},

		// Tuples: (T1, T2, ...)
		Type::Tuple(tuple) => {
			let mut total_size = 0;
			let mut max_align = 1;

			for elem in &tuple.elems {
				let elem_size = calculate_type_size(elem, size_of_types)?;
				let elem_align = get_type_alignment(elem)?;

				// Align current offset to element's alignment
				let padding = if total_size % elem_align == 0 {
					0
				} else {
					elem_align - (total_size % elem_align)
				};

				total_size += padding + elem_size;
				if elem_align > max_align {
					max_align = elem_align;
				}
			}

			// Align total size to max alignment
			if total_size % max_align != 0 {
				total_size += max_align - (total_size % max_align);
			}

			Ok(total_size)
		},

		// Unsupported types
		_ => Err(SynError::new(
			ty.span(),
			"Cannot determine size of complex type. Please use #[size] attribute"
		)),
	}
}

// Get alignment for type using align_of
fn get_type_alignment(ty: &Type) -> Result<usize, SynError> {
	match ty {
		Type::Path(type_path) => {
			if let Some(ident) = type_path.path.get_ident() {
				let type_name = ident.to_string();

				// Handle basic types
				match type_name.as_str() {
					"i8" =>    Ok(align_of::<i8>()),
					"u8" =>    Ok(align_of::<u8>()),
					"i16" =>   Ok(align_of::<i16>()),
					"u16" =>   Ok(align_of::<u16>()),
					"i32" =>   Ok(align_of::<i32>()),
					"u32" =>   Ok(align_of::<u32>()),
					"i64" =>   Ok(align_of::<i64>()),
					"u64" =>   Ok(align_of::<u64>()),
					"f32" =>   Ok(align_of::<f32>()),
					"f64" =>   Ok(align_of::<f64>()),
					"bool" =>  Ok(align_of::<bool>()),
					"char" =>  Ok(align_of::<char>()),
					"isize" => Ok(align_of::<isize>()),
					"usize" => Ok(align_of::<usize>()),
					_ => Ok(8), // Default alignment
				}
			} else {
				Ok(8) // Default alignment for qualified types
			}
		},
		Type::Array(_) => Ok(8), // Default alignment for arrays
		Type::Tuple(_) => Ok(8), // Default alignment for tuples
		_ => Ok(8), // Default alignment for other types
	}
}
