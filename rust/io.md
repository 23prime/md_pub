## IO 関連


### 標準出力

`print!` `println!` というマクロが使える．

```rust
println!("hello, world!");
// => hello, world!
```

`{}` に変数を入れたりできる．

複数の `{}` には，後ろに並べた変数の順に入っていく．

```rust
let a = 0;
let b = 1;
let c = 2;

println!("a is {}, b is {}, c is {}.", a, b, c);
// => a is 0, b is 1, c is 2.
```


### 標準入力

- `std::io::stdin()` の `read_line` が使える．

```rust
let mut s = String::new();
std::io::stdin().read_line(&mut s).unwrap();
// hello =>

assert_eq!(s, "hello\n"); // 改行文字も含まれる
```

改行等の空白文字を落として `FromStr` な任意の型へキャストするには，次のように書けるっぽい．

参考：[【Rust】標準入力から文字列を読み取り、指定の型に変換する関数 - Qiita](https://qiita.com/penguinshunya/items/cd96803b74635aebefd6)

```rust
fn read<T: std::str::FromStr>() -> T {
    let mut s = String::new();
    std::io::stdin().read_line(&mut s).ok();
    s.trim().parse().ok().unwrap()
}

fn read_vec<T: std::str::FromStr>() -> Vec<T> {
    read::<String>()
        .split_whitespace()
        .map(|e| e.parse().ok().unwrap())
        .collect()
}

fn read_vec2<T: std::str::FromStr>(n: u32) -> Vec<Vec<T>> {
    (0..n).map(|_| read_vec()).collect()
}
```


### ファイルの読み書き

参考：[`open` - Rust by Example](http://rust-lang-ja.org/rust-by-example/std_misc/file/open.html)

`File::open` が使える．

例えば，`./docs/sample.csv`
```csv
Sun,17:00,ABC
Mon,24:35,XYZ
```
を `Vec<Vec<&str>>` としてパースしてみる．

```rust
let path = Path::new("./docs/sample.csv");
let display = path.display();

let mut file = match File::open(&path) {
    Err(why) => panic!("couldn't open {}: {}", display, Error::description(&why)),
    // ファイルがなかったりするとエラー吐く
    Ok(file) => file,
};

let mut s = String::new();
let s = match file.read_to_string(&mut s) {
    Err(why) => panic!("couldn't read {}: {}", display, Error::description(&why)),
    // String へ読み込めなかったりするとエラー吐く
    Ok(_) => s,
};

let ss0: Vec<&str> = s.split('\n').collect();
let ss1: Vec<Vec<&str>> = ss0.iter().map(|s| s.split(',').collect()).collect();
assert_eq!(ss0, ["Sun,17:00,ABC", "Mon,24:35,XYZ"]);
assert_eq!(ss1, [["Sun", "17:00", "ABC"], ["Mon", "24:35", "XYZ"]]);
```