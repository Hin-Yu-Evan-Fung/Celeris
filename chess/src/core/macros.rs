/******************************************\
|==========================================|
|     Macro: impl_from_to_primitive      |
|==========================================|
\******************************************/

/// Generates `from_unchecked` and `index` methods for an enum that derives its variants from `u8`. (or from other specified types like i8)
///
/// It provides:
/// - `unsafe fn from_unchecked(index: u8) -> Self`: Converts a `u8` to the enum type. Assumes the index is valid.
/// - `const fn index(&self) -> usize` (Only defined for default u8) : Converts the enum instance to its underlying `usize` value.
///
/// A second variant of the macro allows specifying a different primitive type (e.g., `i8`).
#[macro_export]
macro_rules! impl_from_to_primitive {
    ($enum_name:ident) => {
        impl $enum_name {
            #[doc=concat!("Converts primitive type u8 to ", stringify!($enum_name))]
            /// ## Safety
            /// - This function assumes the inputs are not malformed, so the index has a corresponding enum element with the same discriminator
            #[inline]
            pub const unsafe fn from_unchecked(index: u8) -> Self {
                debug_assert!(index < Self::NUM as u8, "Index out of bounds");
                unsafe { std::mem::transmute(index) }
            }

            #[doc=concat!("Converts ", stringify!($enum_name), " to primitive type usize")]
            #[inline]
            pub const fn index(&self) -> usize {
                *self as usize
            }
        }
    };

    ($enum_name:ident, $type_name:ty) => {
        impl $enum_name {
            #[doc=concat!("Converts primitive type", stringify!($type_name), "to ", stringify!($enum_name))]
            pub const fn from_unchecked(index: $type_name) -> Self {
                unsafe { std::mem::transmute(index) }
            }
        }
    };
}

/******************************************\
|==========================================|
|          Macro: impl_enum_iter           |
|==========================================|
\******************************************/

/// Generates an `iter()` method for an enum that has a `NUM` constant
/// and an `unsafe fn from_unchecked(index: u8) -> Self` method.
/// The `iter()` method returns a `DoubleEndedIterator` over all variants of the enum.
#[macro_export]
macro_rules! impl_enum_iter {
    ($enum_name:ident) => {
        impl $enum_name {
            #[doc=concat!("Returns iterator for all the elements in ", stringify!($enum_name))]
            pub fn iter() -> impl DoubleEndedIterator<Item = Self> {
                (0..Self::NUM as u8).map(|i| unsafe { Self::from_unchecked(i) })
            }
        }
    };
}

/******************************************\
|==========================================|
|             Macro: impl_op               |
|==========================================|
\******************************************/

/// Helper macro to implement a standard binary operator (e.g., `Add`, `BitAnd`)
/// for a struct that wraps a single value (e.g., `MyStruct(value)`).
/// It can implement the operator for `Struct op Struct` or `Struct op OtherType`.
#[macro_export]
macro_rules! impl_op {
    ($struct_name:ident, $op_name:ident, $method_name:ident, $op:tt) => {
        impl std::ops::$op_name for $struct_name {
            type Output = Self;

            fn $method_name(self, rhs: Self) -> Self::Output {
                Self(self.0 $op rhs.0)
            }
        }
    };
    ($struct_name:ident, $op_name:ident, $method_name:ident, $op:tt, $other_type:ident) => {
        impl std::ops::$op_name<$other_type> for $struct_name {
            type Output = Self;

            fn $method_name(self, rhs: $other_type) -> Self::Output {
                Self(self.0 $op rhs)
            }
        }
    }
}

/******************************************\
|==========================================|
|          Macro: impl_assign_op           |
|==========================================|
\******************************************/

/// Helper macro to implement an assignment operator (e.g., `AddAssign`, `BitAndAssign`)
/// for a struct. It assumes the corresponding binary operator is already defined
/// for `Struct op OtherType`.
#[macro_export]
macro_rules! impl_assign_op {
    ($struct_name:ident, $op_name:ident, $method_name:ident, $op:tt, $other_type:ident) => {
        impl std::ops::$op_name<$other_type> for $struct_name {

            fn $method_name(&mut self, rhs: $other_type) {
                *self = *self $op rhs;
            }
        }
    };
}

/******************************************\
|==========================================|
|           Macro: impl_bit_ops            |
|==========================================|
\******************************************/

/// Implements standard bitwise operators (`&`, `|`, `^`) and their
/// corresponding assignment operators (`&=`, `|=`, `^=`) for a struct
/// against another instance of the same struct.
#[macro_export]
macro_rules! impl_bit_ops {
    ($struct_name:ident) => {
        $crate::impl_op!($struct_name, BitAnd, bitand, &);
        $crate::impl_op!($struct_name, BitOr, bitor, |);
        $crate::impl_op!($struct_name, BitXor, bitxor, ^);

        $crate::impl_assign_op!($struct_name, BitAndAssign, bitand_assign, &, $struct_name);
        $crate::impl_assign_op!($struct_name, BitOrAssign, bitor_assign, |, $struct_name);
        $crate::impl_assign_op!($struct_name, BitXorAssign, bitxor_assign, ^, $struct_name);
    };
}

/******************************************\
|==========================================|
|        Macro: impl_bit_mani_ops          |
|==========================================|
\******************************************/

/// Implements bitwise shift operators (`<<`, `>>`), their assignment
/// counterparts (`<<=`, `>>=`), and the unary bitwise `Not` operator (`!`)
/// for a struct. The shift operations are performed against a specified `other_type`
/// (e.g., `u8`).
#[macro_export]
macro_rules! impl_bit_mani_ops {
    ($struct_name:ident, $other_type:ident) => {
        $crate::impl_op!($struct_name, Shl, shl, <<, $other_type);
        $crate::impl_op!($struct_name, Shr, shr, >>, $other_type);

        impl std::ops::Not for $struct_name {
            type Output = Self;

            fn not(self) -> Self::Output {
                Self(!self.0)
            }
        }

        $crate::impl_assign_op!($struct_name, ShlAssign, shl_assign, <<, $other_type);
        $crate::impl_assign_op!($struct_name, ShrAssign, shr_assign, >>, $other_type);
    };
}

/******************************************\
|==========================================|
|           Macro: impl_ari_ops            |
|==========================================|
\******************************************/

/// Implements standard arithmetic operators (`+`, `-`, `*`, `/`) and their
/// corresponding assignment operators (`+=`, `-=`, `*=`, `/=`) for a struct
/// against another instance of the same struct.
#[macro_export]
macro_rules! impl_ari_ops {
    ($struct_name:ident) => {
        $crate::impl_op!($struct_name, Add, add, +);
        $crate::impl_op!($struct_name, Sub, sub, -);
        $crate::impl_op!($struct_name, Mul, mul, *);
        $crate::impl_op!($struct_name, Div, div, /);

        $crate::impl_assign_op!($struct_name, AddAssign, add_assign, +, $struct_name);
        $crate::impl_assign_op!($struct_name, SubAssign, sub_assign, -, $struct_name);
        $crate::impl_assign_op!($struct_name, MulAssign, mul_assign, *, $struct_name);
        $crate::impl_assign_op!($struct_name, DivAssign, div_assign, /, $struct_name);

    }
}
