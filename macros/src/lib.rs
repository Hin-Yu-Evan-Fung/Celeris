use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{Data, DeriveInput, parse_macro_input, parse_str};

#[proc_macro_derive(EnumIter)]
pub fn derive_enum_iter(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);

    // Get the name of the enum
    let name = &input.ident;

    // Extract variant names
    let variants = match &input.data {
        Data::Enum(data) => {
            // Extract the identifiers of the variants
            data.variants.iter().map(|v| &v.ident).collect::<Vec<_>>()
        }
        _ => panic!("Derive FromPrimitive and EnumIter can only be derived for enums"),
    };

    // Get total number of variants
    let n = variants.len();

    // Generate the implementation
    let expanded = quote! {
        impl #name {
            pub const NUM: usize = #n;
            pub fn iter() -> impl DoubleEndedIterator<Item = Self> {
                [
                    #(Self::#variants),*
                ].into_iter()
            }
        }
    };

    // Convert back to token stream
    TokenStream::from(expanded)
}

/// ## Get Representation Type
/// - returns the representation of the enum
fn get_enum_repr_type(ast: &DeriveInput) -> syn::Type {
    // Default to i32 if no repr is specified
    let mut type_name = "i32";

    // Check existing attributes to see if there is a representation
    for attr in &ast.attrs {
        if attr.path().is_ident("repr") {
            let result = attr.parse_nested_meta(|meta| {
                // Handle common primitive integer types
                for name in &[
                    "u8", "i8", "u16", "i16", "u32", "i32", "u64", "i64", "usize", "isize",
                ] {
                    if meta.path.is_ident(name) {
                        type_name = name;
                        return Ok(());
                    }
                }

                // If we get here, it's an unrecognized repr
                Err(meta.error(
                    "Supported repr types are u8, i8, u16, i16, u32, i32, u64, i64, usize, isize",
                ))
            });

            // If we had an error parsing the repr, provide a helpful message
            if let Err(e) = result {
                panic!("Error parsing repr attribute: {}", e);
            }

            // We only care about the first repr attribute
            break;
        }
    }

    // Much simpler way to create a type
    parse_str(type_name).expect("Failed to parse type")
}

#[proc_macro_derive(FromPrimitive)]
pub fn derive_from_primitive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    // Get the name of the enum
    let name = &ast.ident;

    // Get the representation type of the enum
    let repr_type = get_enum_repr_type(&ast);

    // Extract variants
    let enum_data = match &ast.data {
        Data::Enum(data) => data,
        _ => panic!("FromPrimitive can only be derived for enums"),
    };

    // Create match arms for the two implementations
    let from_arms = enum_data.variants.iter().map(|v| {
        let name = &v.ident;

        if let Some((_, expr)) = &v.discriminant {
            // Use explicit discriminant
            quote! { #expr => Self::#name }
        } else {
            // For implicit discriminants, use a cast at runtime
            quote! { v if v == (Self::#name as #repr_type) => Self::#name }
        }
    });

    // Generate the implementation
    let expanded = quote! {
        impl From<#repr_type> for #name {
            fn from(value: #repr_type) -> Self {
                match value {
                    #(#from_arms),*,
                    other => panic!("Invalid value for enum conversion: {}", other),
                }
            }
        }
    };

    TokenStream::from(expanded)
}

/// ## BitOps
fn bit_ops_impl(
    ast: &DeriveInput,
    ops: &[(&str, &str, &str, Option<&str>)],
) -> quote::__private::TokenStream {
    let name = &ast.ident;

    // Get the field type from the struct
    match &ast.data {
        Data::Struct(data_struct) => {
            if data_struct.fields.len() != 1 {
                panic!("BitOps can only be derived for structs with exactly one field");
            }
        }
        _ => panic!("BitOps can only be derived for structs, not enums or unions"),
    };

    // Generate implementations for all operators
    let mut implementations = quote! {};

    // Generate implementations for each operator where Self is on the left
    for (trait_name, op_token, method_name, rhs_type) in ops.iter() {
        let trait_ident = format_ident!("{}", trait_name);
        let trait_assign_ident = format_ident!("{}Assign", trait_name);
        let method_ident = format_ident!("{}", method_name);
        let method_assign_ident = format_ident!("{}_assign", method_name);

        // Parse the operator token
        let op = syn::parse_str::<syn::BinOp>(op_token).unwrap();
        let op_assign = syn::parse_str::<syn::BinOp>(&format!("{}=", op_token)).unwrap();

        // For ops with custom RHS type (like shifts)
        let rhs_type_token = if let Some(t) = rhs_type {
            syn::parse_str::<syn::Type>(t).unwrap()
        } else {
            syn::parse_str::<syn::Type>("Self").unwrap()
        };

        // Only include generic parameters if rhs_type is not Self
        let (op_trait, op_assign_trait) = if let Some(_) = rhs_type {
            (
                quote! { std::ops::#trait_ident<#rhs_type_token> },
                quote! { std::ops::#trait_assign_ident<#rhs_type_token> },
            )
        } else {
            (
                quote! { std::ops::#trait_ident },
                quote! { std::ops::#trait_assign_ident },
            )
        };

        // Use wrapping operations for all operations to prevent overflow
        let wrapping_method = match *method_name {
            "bitand" => format_ident!("bitand"),
            "bitor" => format_ident!("bitor"),
            "bitxor" => format_ident!("bitxor"),
            _ => format_ident!("wrapping_{}", method_name),
        };

        let (op_action, op_assign_action) = if let Some(_) = rhs_type {
            // For bit shift operations
            if *trait_name == "Shl" || *trait_name == "Shr" {
                (
                    quote! { self.0.#wrapping_method(rhs as u32) },
                    quote! { self.0 = self.0.#wrapping_method(rhs as u32) },
                )
            } else {
                // For standard bit operations that don't have wrapping versions
                (quote! { self.0 #op rhs }, quote! { self.0 #op_assign rhs })
            }
        } else {
            // For arithmetic operations (with Self operand)
            if *trait_name == "Add" || *trait_name == "Sub" || *trait_name == "Mul" {
                (
                    quote! { self.0.#wrapping_method(rhs.0) },
                    quote! { self.0 = self.0.#wrapping_method(rhs.0) },
                )
            } else {
                // For standard bit operations that don't have wrapping versions
                (
                    quote! { self.0 #op rhs.0 },
                    quote! { self.0 #op_assign rhs.0 },
                )
            }
        };

        // Regular operator
        let impl_normal = quote! {
            impl #op_trait for #name {
                type Output = Self;

                #[inline]
                fn #method_ident(self, rhs: #rhs_type_token) -> Self::Output {
                    Self(#op_action)
                }
            }
        };

        // Assignment operator
        let impl_assign = quote! {
            impl #op_assign_trait for #name {
                #[inline]
                fn #method_assign_ident(&mut self, rhs: #rhs_type_token) {
                    #op_assign_action;
                }
            }
        };

        implementations = quote! {
            #implementations
            #impl_normal
            #impl_assign
        };
    }

    implementations
}

/// Derives BitOps trait for structs with a single field, implementing
/// common bitwise operations (BitAnd, BitOr, BitXor, and their assignment variants)
#[proc_macro_derive(BitOps)]
pub fn derive_bit_ops(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);

    let implementations = bit_ops_impl(
        &input,
        &[
            ("BitAnd", "&", "bitand", None),
            ("BitOr", "|", "bitor", None),
            ("BitXor", "^", "bitxor", None),
        ],
    );

    TokenStream::from(implementations)
}

#[proc_macro_derive(BitManiOps)]
pub fn derive_bit_mani_ops(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;

    let shift_impls = bit_ops_impl(
        &ast,
        &[
            ("Shl", "<<", "shl", Some("u8")),
            ("Shr", ">>", "shr", Some("u8")),
        ],
    );

    let not_impl = quote! {
        impl std::ops::Not for #name {
            type Output = Self;

            #[inline]
            fn not(self) -> Self::Output {
                Self(!self.0)
            }
        }
    };

    let combined = quote! {
        #shift_impls
        #not_impl
    };

    TokenStream::from(combined)
}

#[proc_macro_derive(AriOps)]
pub fn derive_ari_ops(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let implementations = bit_ops_impl(
        &ast,
        &[
            ("Add", "+", "add", None),
            ("Sub", "-", "sub", None),
            ("Mul", "*", "mul", None),
        ],
    );

    TokenStream::from(implementations)
}
