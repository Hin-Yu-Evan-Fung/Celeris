use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{Data, DeriveInput, parse_macro_input, parse_str};

/// Derives an `iter()` method for an enum, allowing iteration over all its variants.
///
/// This is useful for enums where you need to loop through all possible values,
/// such as iterating over all squares on a chessboard (`Square::iter()`) or all
/// piece types (`PieceType::iter()`).
///
/// # Generated Code Example
///
/// For an enum `MyEnum { A, B }`, this macro generates:
/// ```ignore
/// impl MyEnum {
///     pub fn iter() -> impl DoubleEndedIterator<Item = Self> {
///         [Self::A, Self::B].into_iter()
///     }
/// }
/// ```
///
/// # Panics
///
/// Panics if derived on anything other than an enum.
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
        _ => panic!("Derive EnumIter can only be derived for enums"),
    };

    // Generate the implementation of the iter() method
    let expanded = quote! {
        impl #name {
            /// Returns a double-ended iterator over all variants of this enum.
            ///
            /// The order of variants in the iterator matches their definition order.
            pub fn iter() -> impl DoubleEndedIterator<Item = Self> {
                // Create an array containing all variants
                [
                    #(Self::#variants),*
                ]
                // Convert the array into an iterator
                .into_iter()
            }
        }
    };

    // Convert back to token stream
    TokenStream::from(expanded)
}

/// ## Get Representation Type (Internal Helper)
///
/// Parses the `#[repr(...)]` attribute on an enum definition to determine its
/// underlying primitive integer type.
///
/// This function is used internally by `derive_from_primitive`.
///
/// # Supported Representations
///
/// Supports standard integer types: `u8`, `i8`, `u16`, `i16`, `u32`, `i32`,
/// `u64`, `i64`, `usize`, `isize`.
///
/// # Default
///
/// If no `#[repr(...)]` attribute is found, it defaults to `i32`. This default
/// is generally not suitable for types like `Square` or `Piece` which often
/// use smaller, specific unsigned types like `u8`. **Always specify `#[repr(u8)]`
/// or similar for enums intended for use with `FromPrimitive`.**
///
/// # Returns
///
/// A `syn::Type` representing the determined primitive type.
///
/// # Panics
///
/// - Panics if the `#[repr(...)]` attribute exists but specifies an unsupported type.
/// - Panics if there's an error parsing the type name (which should generally not happen
///   for the hardcoded supported types).
fn get_enum_repr_type(ast: &DeriveInput) -> syn::Type {
    // Default to i32 if no repr is specified (though usually undesirable for this crate)
    let mut type_name = "i32";

    // Check existing attributes to see if there is a representation
    for attr in &ast.attrs {
        if attr.path().is_ident("repr") {
            // Attempt to parse the type specified within #[repr(...)]
            let result = attr.parse_nested_meta(|meta| {
                // Handle common primitive integer types
                for name in &[
                    "u8", "i8", "u16", "i16", "u32", "i32", "u64", "i64", "usize", "isize",
                ] {
                    if meta.path.is_ident(name) {
                        type_name = name;
                        return Ok(()); // Found a supported type
                    }
                }

                // If we get here, it's an unrecognized repr type
                Err(meta.error(
                    "Supported repr types are u8, i8, u16, i16, u32, i32, u64, i64, usize, isize",
                ))
            });

            // If we had an error parsing the repr, provide a helpful message and panic
            if let Err(e) = result {
                panic!("Error parsing repr attribute: {}", e);
            }

            // We only care about the first valid repr attribute found
            break;
        }
    }

    // Parse the determined type name string into a syn::Type
    parse_str(type_name).expect("Internal error: Failed to parse hardcoded type name")
}

/// Derives helper functions for converting between an enum and its primitive representation.
///
/// This macro assumes the enum has a `#[repr(PrimitiveType)]` attribute (e.g., `#[repr(u8)]`).
/// It generates two associated functions within an `impl EnumName { ... }` block:
///
/// 1.  **`unsafe fn from(value: PrimitiveType) -> Self`**:
///     - Performs a highly optimized conversion using `std::mem::transmute`.
///     - **It is `unsafe` because providing a `value` that does not correspond to a valid
///       enum discriminant results in Undefined Behavior in release builds.**
///     - Includes a `debug_assert!` to check validity in debug builds only.
///     - Marked `pub(crate)` to limit its use to within the `chess` crate, encouraging
///       the use of `safe_from` externally or careful validation internally.
///     - **Use with extreme caution.** Only call this if you can mathematically guarantee
///       that `value` is a valid discriminant for the enum.
///
/// 2.  **`fn safe_from(value: PrimitiveType) -> Option<Self>`**:
///     - Performs a safe conversion by checking if the `value` matches any of the
///       enum's valid discriminants using a `match` statement.
///     - Returns `Some(Self)` if the value is valid, and `None` otherwise.
///     - This is the recommended way to convert from a primitive when the input value
///       cannot be guaranteed to be valid.
///
/// # Requirements
///
/// - The target type must be an enum.
/// - The enum must have a `#[repr(PrimitiveType)]` attribute specifying a supported
///   primitive integer type (e.g., `u8`, `i32`).
///
/// # Panics
///
/// - Panics if derived on anything other than an enum.
/// - Panics if the `#[repr(...)]` attribute is missing or invalid (via `get_enum_repr_type`).
#[proc_macro_derive(FromPrimitive)]
pub fn derive_from_primitive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    // Get the name of the enum
    let name = &ast.ident;

    // Get the representation type of the enum (e.g., u8, i32)
    let repr_type = get_enum_repr_type(&ast);

    // Extract enum variant data
    let enum_data = match &ast.data {
        Data::Enum(data) => data,
        _ => panic!("Derive FromPrimitive can only be derived for enums"),
    };

    // --- Prepare data for `safe_from` ---
    // Create match arms for the `safe_from` implementation.
    // Handles both explicit discriminants (e.g., `Variant = 10`) and implicit ones.
    let from_arms = enum_data.variants.iter().map(|v| {
        let variant_name = &v.ident;

        if let Some((_, expr)) = &v.discriminant {
            // Explicit discriminant: `10 => Some(Self::Variant)`
            quote! { #expr => Some(Self::#variant_name) }
        } else {
            // Implicit discriminant: needs runtime check `v if v == (Self::Variant as #repr_type) => Some(Self::Variant)`
            // This relies on the compiler assigning discriminants sequentially as expected.
            quote! { v if v == (Self::#variant_name as #repr_type) => Some(Self::#variant_name) }
        }
    });

    // --- Prepare data for `unsafe fn from`'s debug_assert ---
    // Create expressions representing the valid discriminant values.
    // These can be literals or runtime casts for implicit discriminants.
    let discriminant_values = enum_data.variants.iter().map(|v| {
        if let Some((_, expr)) = &v.discriminant {
            // Use explicit discriminant expression (e.g., `10`)
            quote! { #expr }
        } else {
            // For implicit discriminants, use a cast at runtime (e.g., `Self::Variant as u8`)
            quote! { Self::#v as #repr_type }
        }
    });

    // Build the boolean expression for the debug_assert.
    // Handles the edge case of an empty enum (generates `false`).
    let check_expression = if enum_data.variants.is_empty() {
        quote! { false } // An empty enum has no valid values
    } else {
        // Generate: `(value == discr1) || (value == discr2) || ...`
        quote! {
            #( (value == #discriminant_values) )||*
        }
    };

    // --- Generate the `impl` block ---
    let expanded = quote! {
        impl #name {
            /// Creates an enum variant from its primitive representation using `transmute`.
            ///
            /// **This function is highly unsafe.** It performs **no checks** in release builds.
            /// Providing a `value` that does not correspond to a valid discriminant for this
            /// enum will result in **Undefined Behavior**.
            ///
            /// It is marked `pub(crate)` to discourage external use. Prefer `safe_from` unless
            /// you have benchmarked and proven a critical need for this function, and can
            /// absolutely guarantee the validity of the input `value`.
            ///
            /// # Safety
            ///
            /// The caller *must* ensure that `value` represents a valid discriminant for
            /// the enum `Self`. In debug builds (`debug_assertions` enabled), a `debug_assert!`
            /// will check this condition at runtime, but this check is **removed** in release builds.
            ///
            /// # Panics
            ///
            /// Panics in debug builds if `value` is not a valid discriminant.
            #[inline]
            pub const unsafe fn from_unchecked(value: #repr_type) -> Self {
                // Check validity ONLY in debug builds. This compiles away in release.
                debug_assert!(
                    #check_expression,
                    "Invalid value conversion from primitive type"
                );

                // SAFETY: Relies entirely on the caller upholding the safety contract
                // in release builds. The `debug_assert` provides a check in debug builds.
                // `transmute` reinterprets the bits of `value` as `Self`.
                std::mem::transmute::<#repr_type, Self>(value)
            }

            /// Safely creates an enum variant from its primitive representation.
            ///
            /// This function checks if the given `value` corresponds to any valid discriminant
            /// of the enum.
            ///
            /// # Arguments
            ///
            /// * `value`: The primitive value (`#repr_type`) to convert.
            ///
            /// # Returns
            ///
            /// * `Some(Self)` if `value` is a valid discriminant for this enum.
            /// * `None` if `value` does not correspond to any variant.
            #[inline]
            pub const fn from(value: #repr_type) -> Option<Self> {
                match value {
                    // Expand the generated match arms: e.g., `0 => Some(Self::VariantA), ...`
                    #(#from_arms),*,
                    // If no arm matches, the value is invalid
                    _ => None,
                }
            }
        }
    };

    TokenStream::from(expanded)
}

/// ## Bit Operations Implementation Helper (Internal)
///
/// Generates `impl` blocks for standard binary operators (like `BitAnd`, `Add`, `Shl`)
/// and their corresponding assignment operators (`BitAndAssign`, `AddAssign`, `ShlAssign`)
/// for a struct with a single field.
///
/// This function is a private implementation detail used by the public `derive` macros
/// (`BitOps`, `BitManiOps`, `AriOps`).
///
/// # Arguments
///
/// * `ast`: The parsed `DeriveInput` for the struct.
/// * `ops`: A slice defining the operators to implement. Each tuple contains:
///     - `trait_name`: The name of the main trait (e.g., "BitAnd", "Add").
///     - `op_token`: The operator symbol as a string (e.g., "&", "+").
///     - `method_name`: The name of the trait method (e.g., "bitand", "add").
///     - `rhs_type`: An `Option<&str>` specifying the right-hand side type.
///     - `None` means `Self`, `Some("u8")` means `u8`, etc. Used for shifts.
///
/// # Behavior
///
/// - Assumes the struct has exactly one field (usually `pub struct Name(Type)`).
/// - Generates implementations for both `Trait` and `TraitAssign`.
/// - Uses **wrapping** arithmetic (`wrapping_add`, `wrapping_sub`, `wrapping_mul`) for
///   Add, Sub, Mul to prevent panics on overflow.
/// - Uses standard bitwise operators for And, Or, Xor, Not.
/// - Handles shift operations (`Shl`, `Shr`) potentially taking a different RHS type (e.g., `u8`).
///
/// # Returns
///
/// A `TokenStream` containing the generated `impl` blocks.
///
/// # Panics
///
/// - Panics if the input `ast` is not a struct.
/// - Panics if the struct does not have exactly one field.
/// - Panics if operator strings cannot be parsed (should not happen with valid input).
fn bit_ops_impl(
    ast: &DeriveInput,
    ops: &[(&str, &str, &str, Option<&str>)],
) -> quote::__private::TokenStream {
    let name = &ast.ident;

    // Ensure it's a struct with exactly one field
    match &ast.data {
        Data::Struct(data_struct) => {
            if data_struct.fields.len() != 1 {
                panic!(
                    "BitOps/AriOps/BitManiOps can only be derived for structs with exactly one field (e.g., struct MyType(u64))"
                );
            }
        }
        _ => {
            panic!("BitOps/AriOps/BitManiOps can only be derived for structs, not enums or unions")
        }
    };

    // Generate implementations for all requested operators
    let mut implementations = quote! {};

    for (trait_name, op_token, method_name, rhs_type_str) in ops.iter() {
        // Create identifiers for traits and methods
        let trait_ident = format_ident!("{}", trait_name); // e.g., BitAnd
        let trait_assign_ident = format_ident!("{}Assign", trait_name); // e.g., BitAndAssign
        let method_ident = format_ident!("{}", method_name); // e.g., bitand
        let method_assign_ident = format_ident!("{}_assign", method_name); // e.g., bitand_assign

        // Parse the operator tokens (e.g., "&", "&=")
        let op = syn::parse_str::<syn::BinOp>(op_token)
            .expect("Internal macro error: Invalid operator token");
        let op_assign = syn::parse_str::<syn::BinOp>(&format!("{}=", op_token))
            .expect("Internal macro error: Invalid assignment operator token");

        // Determine the RHS type (Self or a specified type like u8)
        let rhs_type_token = if let Some(t) = rhs_type_str {
            syn::parse_str::<syn::Type>(t).expect("Internal macro error: Invalid RHS type string")
        } else {
            syn::parse_str::<syn::Type>("Self").unwrap() // Default to Self
        };

        // Determine the fully qualified trait paths, including generics if RHS is not Self
        let (op_trait, op_assign_trait) = if rhs_type_str.is_some() {
            // e.g., std::ops::Shl<u8>, std::ops::ShlAssign<u8>
            (
                quote! { std::ops::#trait_ident<#rhs_type_token> },
                quote! { std::ops::#trait_assign_ident<#rhs_type_token> },
            )
        } else {
            // e.g., std::ops::BitAnd, std::ops::BitAndAssign
            (
                quote! { std::ops::#trait_ident },
                quote! { std::ops::#trait_assign_ident },
            )
        };

        // Select appropriate method call: wrapping for arithmetic, direct for bitwise/shifts
        // Note: Bitwise ops (&, |, ^) don't have wrapping versions and don't panic.
        let wrapping_method = match *method_name {
            "add" | "sub" | "mul" | "shl" | "shr" => format_ident!("wrapping_{}", method_name),
            // Use direct method names for bitwise ops and shifts
            "bitand" | "bitor" | "bitxor" => method_ident.clone(),
            _ => panic!(
                "Internal macro error: Unsupported method name {}",
                method_name
            ),
        };

        // Generate the code snippets for the operation itself
        let (op_action, op_assign_action) = if rhs_type_str.is_some() {
            // Case 1: RHS is a specific type (e.g., u8 for shifts)
            // Shifts require casting RHS to u32 as per Rust's shift trait methods
            if *trait_name == "Shl" || *trait_name == "Shr" {
                (
                    quote! { self.0.#wrapping_method(rhs as u32) }, // e.g., self.0.shl(rhs as u32)
                    quote! { self.0 = self.0.#wrapping_method(rhs as u32) }, // e.g., self.0 = self.0.shl(rhs as u32)
                )
            } else {
                // Should not happen with current ops config, but handle generic case
                (quote! { self.0 #op rhs }, quote! { self.0 #op_assign rhs })
            }
        } else {
            // Case 2: RHS is Self
            if *trait_name == "Add" || *trait_name == "Sub" || *trait_name == "Mul" {
                // Use wrapping methods for arithmetic
                (
                    quote! { self.0.#wrapping_method(rhs.0) }, // e.g., self.0.wrapping_add(rhs.0)
                    quote! { self.0 = self.0.#wrapping_method(rhs.0) }, // e.g., self.0 = self.0.wrapping_add(rhs.0)
                )
            } else {
                // Use direct operators for bitwise operations (And, Or, Xor)
                (
                    quote! { self.0 #op rhs.0 },        // e.g., self.0 & rhs.0
                    quote! { self.0 #op_assign rhs.0 }, // e.g., self.0 &= rhs.0
                )
            }
        };

        // Generate the impl block for the standard operator (e.g., impl BitAnd for Name)
        let impl_normal = quote! {
            impl #op_trait for #name {
                type Output = Self; // Output is always Self for these ops

                #[inline]
                fn #method_ident(self, rhs: #rhs_type_token) -> Self::Output {
                    // Construct a new Self instance with the result
                    Self(#op_action)
                }
            }

            impl #name {
                #[inline]
                pub const fn #method_ident(self, rhs: #rhs_type_token) -> Self {
                    Self(#op_action)
                }
            }
        };

        // Generate the impl block for the assignment operator (e.g., impl BitAndAssign for Name)
        let impl_assign = quote! {
            impl #op_assign_trait for #name {
                #[inline]
                fn #method_assign_ident(&mut self, rhs: #rhs_type_token) {
                    // Modify self in place
                    #op_assign_action;
                }
            }

            impl #name {
                #[inline]
                pub const fn #method_assign_ident(&mut self, rhs: #rhs_type_token) {
                    #op_assign_action;
                }
            }
        };

        // Append the generated impls to the overall output
        implementations = quote! {
            #implementations
            #impl_normal
            #impl_assign
        };
    }

    implementations
}

/// Derives standard bitwise operations (`&`, `|`, `^`) and their assignment counterparts
/// (`&=`, `|=`, `^=`) for a struct with a single numeric field.
///
/// This is typically used for types representing bitmasks or bitboards, like `Bitboard` or `Key`.
///
/// # Requirements
///
/// - The target must be a struct with exactly one field (e.g., `struct MyBits(u64)`).
/// - The single field's type must support the standard bitwise operators.
///
/// # Generated Traits
///
/// - `std::ops::BitAnd`
/// - `std::ops::BitAndAssign`
/// - `std::ops::BitOr`
/// - `std::ops::BitOrAssign`
/// - `std::ops::BitXor`
/// - `std::ops::BitXorAssign`
///
/// # Example
///
/// ```ignore
/// #[derive(BitOps)]
/// struct Flags(u8);
///
/// let f1 = Flags(0b0011);
/// let f2 = Flags(0b0110);
/// assert_eq!((f1 & f2).0, 0b0010);
/// assert_eq!((f1 | f2).0, 0b0111);
/// assert_eq!((f1 ^ f2).0, 0b0101);
/// ```
#[proc_macro_derive(BitOps)]
pub fn derive_bit_ops(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);

    // Use the internal helper function to generate the implementations
    let implementations = bit_ops_impl(
        &input,
        &[
            // Define the BitAnd, BitOr, BitXor operations (RHS is Self)
            ("BitAnd", "&", "bitand", None),
            ("BitOr", "|", "bitor", None),
            ("BitXor", "^", "bitxor", None),
        ],
    );

    TokenStream::from(implementations)
}

/// Derives bit manipulation operations: shifts (`<<`, `>>`) and bitwise NOT (`!`).
///
/// Assumes the right-hand side of shift operations is `u8`.
///
/// # Requirements
///
/// - The target must be a struct with exactly one field (e.g., `struct MyBits(u64)`).
/// - The single field's type must support shift (`shl`, `shr`) and not (`!`) operations.
///
/// # Generated Traits
///
/// - `std::ops::Shl<u8>`
/// - `std::ops::ShlAssign<u8>`
/// - `std::ops::Shr<u8>`
/// - `std::ops::ShrAssign<u8>`
/// - `std::ops::Not`
///
/// # Example
///
/// ```ignore
/// #[derive(BitManiOps)]
/// struct Bitboard(u64);
///
/// let b = Bitboard(1);
/// assert_eq!((b << 1u8).0, 2);
/// assert_eq!((b << 8u8).0, 256);
/// assert_eq!((Bitboard(255) >> 4u8).0, 15);
/// assert_eq!((!Bitboard(0)).0, u64::MAX);
/// ```
#[proc_macro_derive(BitManiOps)]
pub fn derive_bit_mani_ops(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;

    // Generate shift implementations using the helper (RHS is u8)
    let shift_impls = bit_ops_impl(
        &ast,
        &[
            ("Shl", "<<", "shl", Some("u8")),
            ("Shr", ">>", "shr", Some("u8")),
        ],
    );

    // Generate the implementation for `Not` separately
    let not_impl = quote! {

        impl std::ops::Not for #name {
            type Output = Self;
            #[inline]
            fn not(self) -> Self {
                // Apply NOT to the inner field and wrap in Self
                Self(!self.0)
            }
        }


        impl #name {
            #[inline]
            pub const fn not(self) -> Self {
                // Apply NOT to the inner field and wrap in Self
                Self(!self.0)
            }
        }
    };

    // Combine shift and not implementations
    let combined = quote! {
        #shift_impls
        #not_impl
    };

    TokenStream::from(combined)
}

/// Derives standard arithmetic operations (`+`, `-`, `*`) and their assignment counterparts
/// (`+=`, `-=`, `*=`) for a struct with a single numeric field, using **wrapping** semantics.
///
/// Wrapping arithmetic means that operations that would overflow will wrap around
/// (e.g., `u8::MAX + 1` becomes `0`) instead of panicking (in debug builds) or
/// producing unexpected results (in release builds with default arithmetic).
///
/// # Requirements
///
/// - The target must be a struct with exactly one field (e.g., `struct Counter(u32)`).
/// - The single field's type must support wrapping arithmetic methods
///   (`wrapping_add`, `wrapping_sub`, `wrapping_mul`).
///
/// # Generated Traits
///
/// - `std::ops::Add`
/// - `std::ops::AddAssign`
/// - `std::ops::Sub`
/// - `std::ops::SubAssign`
/// - `std::ops::Mul`
/// - `std::ops::MulAssign`
///
/// # Example
///
/// ```ignore
/// #[derive(AriOps, PartialEq, Debug)]
/// struct WrappedCounter(u8);
///
/// let mut c = WrappedCounter(254);
/// assert_eq!(c + WrappedCounter(1), WrappedCounter(255));
/// assert_eq!(c + WrappedCounter(2), WrappedCounter(0)); // Wraps around
/// c += WrappedCounter(3);
/// assert_eq!(c, WrappedCounter(1)); // 254 + 3 -> 257 -> wraps to 1
/// ```
#[proc_macro_derive(AriOps)]
pub fn derive_ari_ops(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    // Use the internal helper function to generate the implementations
    let implementations = bit_ops_impl(
        &ast,
        &[
            // Define Add, Sub, Mul operations (RHS is Self), using wrapping methods
            ("Add", "+", "add", None),
            ("Sub", "-", "sub", None),
            ("Mul", "*", "mul", None),
        ],
    );

    TokenStream::from(implementations)
}
