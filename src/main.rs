use swat::parser::parse;

fn main() {
    let bytes = include_bytes!("../target/wasm32-unknown-unknown/release/swat.wasm");
    let module = parse(bytes).unwrap();

    println!("{:?}", module);
}
