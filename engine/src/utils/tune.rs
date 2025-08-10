#[macro_export]
macro_rules! init_tunables {
    ($($name:ident: $t:ty = $val:expr, $min:expr, $max:expr, $step:expr;)*) => {
        pub mod tunables {
            #[cfg(feature = "tune")]
            mod storage {
                use std::sync::atomic::AtomicI32;
                $(
                    #[allow(non_upper_case_globals)]
                    pub static $name: AtomicI32 = AtomicI32::new($val as i32);
                )*
            }

            $(
                #[cfg(not(feature = "tune"))]
                #[inline]
                pub const fn $name() -> $t {
                    $val
                }

                #[cfg(feature = "tune")]
                #[inline]
                pub fn $name() -> $t {
                    use std::sync::atomic::Ordering;
                    storage::$name.load(Ordering::Relaxed) as $t
                }
            )*

            #[cfg(feature = "tune")]
            pub fn set_tunable(tunable_name: &str, val: &str) -> Result<(), &'static str> {
                use std::sync::atomic::Ordering;
                match tunable_name {
                    $(stringify!($name) => {
                        let parsed: i32 = val.parse().map_err(|_| "Invalid value")?;
                        storage::$name.store(parsed, Ordering::Relaxed);
                        Ok(())
                    },)*
                    _ => Err("Unknown option!")
                }
            }

            #[cfg(feature = "tune")]
            pub fn spsa_output_opts() -> String {
                let mut options = String::new();
                $(
                    options.push_str(&format!(
                        "option name {} type spin default {} min {} max {}\n",
                        stringify!($name),
                        $val,
                        $min,
                        $max,
                    ));
                )*
                options
            }

            #[cfg(feature = "tune")]
            pub fn spsa_output_txt() -> String {
                let mut txt = String::new();
                $(
                    txt.push_str(&format!(
                        "{}, int, {}.0, {}.0, {}.0, {}.0, 0.002\n",
                        stringify!($name),
                        $val,
                        $min,
                        $max,
                        $step,
                    ));
                )*
                txt
            }
        }
    }
}
