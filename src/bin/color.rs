fn main() {
    use tcolor::*;
    let s = ColorString::new_fg("\"ejiofw\"", (Color::BrightGreen, Color::BrightBlue));
    println!("escaped:        {:?}", s.to_string());
    println!("rendered:       {}", s);

    let manual = "\u{1b}[92m\"ejiofw\"\u{1b}[m";
    println!("manual escaped: {manual:?}");
    println!("manual:         {manual}");
}
