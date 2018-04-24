## 文字列

### 文字列型

2つの文字列型がある．
- `&str` : 固定長．文字列スライスっていうらしい．
- `String` : 可変長．`to_string()` によって `&str` から変換できる．逆に，`&` によって `&str` に変換できる．

### index によるアクセス
```rust
let s = "hello";

let t = &s[0..5];
assert_eq!(t, "hello");

let cs: Vec<char> = s.chars().collect(); // Char の Vec にバラす
assert_eq!(Vec::len(&cs), 5);
assert_eq!(cs[0], 'h');
```

日本語だと次のような感じ．
```rust
let mut s = "ハロー！";

let t = &s[0..12]; // Byte で当たるので注意
assert_eq!(t, "ハロー！");

let cs: Vec<char> = s.chars().collect();
assert_eq!(Vec::len(&cs), 4);
assert_eq!(cs[0], 'ハ');
```

### 文字列の連結
- `push(&mut self, ch: char)`: `Char` を後方に連結．
- `push_str(&mut self, string: &str)`: `&str` を後方に連結．
- `+`: `push_str` とほぼ同じ．
- `concat()`: `Vec<&str>` の要素を連結する．
- `connect(s: &str)`: `Vec<&str>` の要素に `s` を挟んで連結する．  
が使える．

```rust
let mut h = "hello,".to_string();
let s = " ";
let w = "world!";
assert_eq!(h + s + w, "hello, world!");
```

```rust
let mut ss: Vec<&str> = vec!["hello,", " ", "world!"];
let s = ss.concat();
assert_eq!(s, "hello, world!");
```

```rust
let mut ss: Vec<&str> = vec!["hello,", "world!"];
let s = ss.connect(" ");
assert_eq!(s, "hello, world!");
```

### 文字列の分割
- `split`: `Char` で分割．
- `split_str`: `&str` で分割．

```rust
let mut s = "a b c d e";
let ss: Vec<&str> = s.split(' ').collect();
assert_eq!(ss, vec!["a", "b", "c", "d", "e"]);
```


### 文字列の長さ（つまり文字数）とキャパシティ

- `len(&self) -> usize`
- `capacity(&self) -> usize`

```rust
let mut h = "hello,".to_string();
let s = ' ';
let w = "world!";

let mut len = h.len();
let mut cap = h.capacity();
assert_eq!(len, 6);
assert_eq!(cap, 6);

h.push(s);
assert_eq!(h, "hello, ");
let mut len = h.len();
let mut cap = h.capacity();
assert_eq!(len, 7);
assert_eq!(cap, 12);

h += w;
assert_eq!(h, "hello, world!");
let mut len = h.len();
let mut cap = h.capacity();
assert_eq!(len, 13);
assert_eq!(cap, 24);
``` 

### 文字列の置換

- `replace(s0, s1)`: `s0` を `s1` へ置換．

```rust
let s = "hello, world!";
let r = s.replace("world", "hello");
assert_eq!(s, "hello, world!");
assert_eq!(r, "hello, hello!");
```