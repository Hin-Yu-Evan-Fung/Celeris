pub(crate) const fn abs_diff(a: u8, b: u8) -> u8 {
    if a > b { a - b } else { b - a }
}
