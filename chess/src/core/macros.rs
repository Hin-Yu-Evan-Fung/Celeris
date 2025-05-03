#[macro_export]
macro_rules! impl_from_to_primitive {
    ($enum_name:ident) => {
        impl $enum_name {
            /// Converts u8 to Rank
            pub const fn from_unchecked(index: u8) -> Self {
                debug_assert!(index < Self::NUM as u8, "Index out of bounds");
                unsafe { std::mem::transmute(index) }
            }

            pub const fn index(&self) -> usize {
                *self as usize
            }
        }
    };

    ($enum_name:ident, $type_name:ty) => {
        impl $enum_name {
            /// Converts $type to Rank
            pub const fn from_unchecked(index: $type_name) -> Self {
                unsafe { std::mem::transmute(index) }
            }
        }
    };
}

#[macro_export]
macro_rules! impl_enum_iter {
    ($enum_name:ident) => {
        impl $enum_name {
            /// Returns an iterator for the enum
            pub fn iter() -> impl DoubleEndedIterator<Item = Self> {
                (0..Self::NUM as u8).map(Self::from_unchecked)
            }
        }
    };
}

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

#[macro_export]
macro_rules! impl_ari_ops {
    ($struct_name:ident) => {
        $crate::impl_op!($struct_name, Add, add, +);
        $crate::impl_op!($struct_name, Sub, sub, -);
        $crate::impl_op!($struct_name, Mul, mul, *);

        $crate::impl_assign_op!($struct_name, AddAssign, add_assign, +, $struct_name);
        $crate::impl_assign_op!($struct_name, SubAssign, sub_assign, -, $struct_name);
        $crate::impl_assign_op!($struct_name, MulAssign, mul_assign, *, $struct_name);
    }
}
