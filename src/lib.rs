
use proc_macro::TokenStream;
use proc_macro2::{
	TokenStream as TokenStream2,
};
use quote::{
	quote,
	format_ident,
};
use syn::{
	Attribute,
	DeriveInput,
	Expr,
	Generics,
	Ident,
	ImplGenerics,
	Lit,
	Meta,
	Type,
	TypeGenerics,
	WhereClause,
	parse_macro_input,
};

/// Alignment configuration for a field
#[derive(Debug, Clone, Default)]
struct FieldAlignment {
	align: Option<usize>,
	packed: bool,
}

/// Alignment configuration for a struct
#[derive(Debug, Clone, Default)]
struct StructAlignment {
	align: Option<usize>,
	packed: bool,
}

/// Field information
#[derive(Debug)]
struct FieldInfo {
	ident: String,
	ty: Type,
	alignment: FieldAlignment,
	is_padding: bool,
}

#[proc_macro_derive(FFIStruct, attributes(ffi_struct, align, repr))]
pub fn derive_ffi_struct(input: TokenStream) -> TokenStream {
	let input = parse_macro_input!(input as DeriveInput);
	
	// Process struct name and generics
	let name = &input.ident;
	let generics = add_trait_bounds(input.generics);
	let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
	
	// Parse struct alignment attributes
	let struct_alignment = parse_struct_alignment(&input.attrs);
	
	// Parse field information
	let mut fields_info = parse_fields(&input.data);
	
	// Calculate and insert padding fields
	calculate_padding(&mut fields_info, &struct_alignment);
	
	// Generate normal field list (excluding padding)
	let normal_fields = fields_info.iter()
		.filter(|f| !f.is_padding)
		.collect::<Vec<_>>();
	
	// Generate all field list (including padding)
	let all_fields = fields_info.iter().collect::<Vec<_>>();
	
	// Generate implementation code
	let expanded = generate_impl(
		name,
		&impl_generics,
		&ty_generics,
		where_clause,
		&struct_alignment,
		&normal_fields,
		&all_fields,
	);
	
	expanded.into()
}

/// Add trait bounds for generics
fn add_trait_bounds(mut generics: Generics) -> Generics {
	for param in &mut generics.params {
		if let syn::GenericParam::Type(type_param) = param {
			type_param.bounds.push(syn::parse_quote!(FFIStruct));
		}
	}
	generics
}

/// Parse struct alignment attributes
fn parse_struct_alignment(attrs: &[Attribute]) -> StructAlignment {
	let mut config = StructAlignment::default();
	
	for attr in attrs {
		if attr.path().is_ident("repr") {
			if let Ok(Meta::List(list)) = attr.parse_args() {
				for token in list.tokens {
					let token_str = token.to_string();
					if token_str.starts_with("align") {
						if let Some(align_str) = token_str.split('(').nth(1) {
							if let Some(align_str) = align_str.strip_suffix(')') {
								if let Ok(align) = align_str.parse::<usize>() {
									config.align = Some(align);
								}
							}
						}
					} else if token_str == "packed" {
						config.packed = true;
					}
				}
			}
		}
	}
	
	config
}

/// Parse field information and alignment attributes
fn parse_fields(data: &syn::Data) -> Vec<FieldInfo> {
	let fields = match data {
		syn::Data::Struct(s) => &s.fields,
		_ => panic!("Only structs are supported"),
	};
	
	fields.iter().enumerate().map(|(i, field)| {
		let ident = match &field.ident {
			Some(ident) => ident.to_string(),
			None => format!("field_{}", i),
		};
		
		// Parse field alignment attributes
		let mut alignment = FieldAlignment::default();
		for attr in &field.attrs {
			if attr.path().is_ident("align") {
				if let Meta::NameValue(ref nv) = attr.meta {
					if let Expr::Lit(ref lit) = nv.value {
						if let Lit::Int(ref int) = lit.lit {
							if let Ok(align) = int.base10_parse::<usize>() {
								alignment.align = Some(align);
							}
						}
					}
				}
			} else if attr.path().is_ident("packed") {
				alignment.packed = true;
			}
		}
		
		FieldInfo {
			ident,
			ty: field.ty.clone(),
			alignment,
			is_padding: false,
		}
	}).collect()
}

/// Calculate and insert padding fields based on alignment requirements
fn calculate_padding(
	fields: &mut Vec<FieldInfo>,
	struct_alignment: &StructAlignment,
) -> Vec<TokenStream2> {
	let mut padding_fields = Vec::new();
	let mut current_offset = 0;
	let mut padding_index = 0u32;
	
	// Calculate max alignment requirement
	let max_align = fields.iter()
		.filter_map(|f| f.alignment.align)
		.chain(struct_alignment.align)
		.max()
		.unwrap_or(1);
	
	for i in 0..fields.len() {
		let field = &fields[i];
		
		// Get field alignment requirement
		let field_align = if struct_alignment.packed {
			1
		} else {
			field.alignment.align
				.or(struct_alignment.align)
				.unwrap_or(max_align)
		};
		
		// Calculate padding needed
		let padding_needed = (field_align - (current_offset % field_align)) % field_align;
		
		if padding_needed > 0 {
			// Create padding field
			let padding_type = get_padding_type(padding_needed);
			let padding_ident = format_ident!("_pad{}", padding_index);
			padding_index += 1;
			
			// Insert padding field
			fields.insert(i, FieldInfo {
				ident: padding_ident.to_string(),
				ty: padding_type.clone(),
				alignment: FieldAlignment::default(),
				is_padding: true,
			});
			
			// Record padding field code
			padding_fields.push(quote! {
				#padding_ident: #padding_type,
			});
			
			// Update offset
			current_offset += padding_needed;
		}
		
		// Update offset with field size (simplified)
		current_offset += 8; // Placeholder for actual size calculation
	}
	
	// Add tail padding if needed
	if !struct_alignment.packed {
		let tail_padding = (max_align - (current_offset % max_align)) % max_align;
		if tail_padding > 0 {
			let padding_type = get_padding_type(tail_padding);
			let padding_ident = format_ident!("_tail_pad");
			
			fields.push(FieldInfo {
				ident: padding_ident.to_string(),
				ty: padding_type.clone(),
				alignment: FieldAlignment::default(),
				is_padding: true,
			});
			
			padding_fields.push(quote! {
				#padding_ident: #padding_type,
			});
		}
	}
	
	padding_fields
}

/// Get appropriate padding type based on size
fn get_padding_type(size: usize) -> Type {
	match size {
		0 => syn::parse_str("()").unwrap(),
		1 => syn::parse_str("u8").unwrap(),
		2 => syn::parse_str("u16").unwrap(),
		3 => syn::parse_str("[u8; 3]").unwrap(),
		4 => syn::parse_str("u32").unwrap(),
		_ => syn::parse_str(&format!("[u8; {}]", size)).unwrap(),
	}
}

/// Generate implementation code
fn generate_impl(
	name: &Ident,
	impl_generics: &ImplGenerics,
	ty_generics: &TypeGenerics,
	where_clause: Option<&WhereClause>,
	struct_alignment: &StructAlignment,
	normal_fields: &[&FieldInfo],
	all_fields: &[&FieldInfo],
) -> TokenStream2 {
	// Generate struct alignment attribute
	let repr_attr = if struct_alignment.packed {
		quote! { #[repr(packed)] }
	} else if let Some(align) = struct_alignment.align {
		quote! { #[repr(align(#align))] }
	} else {
		quote! { #[repr(C)] }
	};
	
	// Generate normal field info
	let normal_field_info = normal_fields.iter().map(|field| {
		let ident = &field.ident;
		let ty = &field.ty;
		quote! {
			::ffi_struct::FieldInfo {
				name: #ident,
				type_id: std::any::TypeId::of::<#ty>(),
				type_name: stringify!(#ty),
				size: std::mem::size_of::<#ty>(),
				is_padding: false,
			}
		}
	});
	
	// Generate all field info (including padding)
	let all_field_info = all_fields.iter().map(|field| {
		let ident = &field.ident;
		let ty = &field.ty;
		let is_padding = field.is_padding;
		quote! {
			::ffi_struct::FieldInfo {
				name: #ident,
				type_id: std::any::TypeId::of::<#ty>(),
				type_name: stringify!(#ty),
				size: std::mem::size_of::<#ty>(),
				is_padding: #is_padding,
			}
		}
	});
	
	// Generate field iteration code
	let normal_iter = normal_fields.iter().map(|field| {
		let ident_str = &field.ident;
		let ident = format_ident!("{}", field.ident);
		quote! {
			f(#ident_str, &self.#ident);
		}
	});
	
	let all_iter = all_fields.iter().map(|field| {
		let ident_str = &field.ident;
		let ident = format_ident!("{}", field.ident);
		quote! {
			f(#ident_str, &self.#ident);
		}
	});
	
	quote! {
		#repr_attr
		
		impl #impl_generics ::ffi_struct::FFIStruct for #name #ty_generics #where_clause {
			/// Get struct alignment
			fn alignment() -> usize {
				std::mem::align_of::<Self>()
			}
			
			/// Get field info (excluding padding)
			fn field_info() -> Vec<::ffi_struct::FieldInfo> {
				vec![
					#( #normal_field_info ),*
				]
			}
			
			/// Get all field info (including padding)
			fn all_field_info() -> Vec<::ffi_struct::FieldInfo> {
				vec![
					#( #all_field_info ),*
				]
			}
			
			/// Iterate fields (excluding padding)
			fn iterate_fields(&self, mut f: impl FnMut(&str, &dyn std::any::Any)) {
				#( #normal_iter )*
			}
			
			/// Iterate all fields (including padding)
			fn iterate_all_fields(&self, mut f: impl FnMut(&str, &dyn std::any::Any)) {
				#( #all_iter )*
			}
		}
	}
}
