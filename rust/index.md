# Rust

Note for Rust.

## Compared with Haskell.

### Struct

円を表す型を定義してみる．

Rust
```rust
struct Circle {
    x: f64,
    y: f64,
    r: f64,
}
```

Haskell
```haskell
data Circle = C { x :: Double
                , y :: Double
                , r :: Double
                }
```

### Trait
Haskell でいう型クラス．

面積を持つ Trait を定義してみる．

Rust
```rust
trait HasArea {
    fn area(&self) -> f64;
}
```

Haskell
```haskell
class HasArea a where
  area :: a -> Double
```

### Implementation

Haskell でいうインスタンス．

Rust
```rust
impl HasArea for Circle {
    fn area(&self) -> f64 {
        std::f64::consts::PI * (self.r * self.r)
    }
}
```

Haskell
```haskell
instance HasArea Circle where
  area (C x y r) = pi * r ^ 2
```

### Trait による型制約

円の面積を表示する．

Rust
```rust
fn print_area<T: HasArea>(shape: T) {
    let a = shape.area();
    println!("This shape has an area of {}.", a);
}
```

Haskell
```haskell
printArea :: HasArea a => a -> IO ()
printArea f = do
  let a = show $ area f
  putStrLn $ "This shape has an area of " ++ a
```

### Derive

型の定義に対する Implementation の自動導出．等値性を持たせてみる．

Rust
```rust
#[derive(PartialEq)]
struct Circle {
    x: f64,
    y: f64,
    r: f64,
}
```

Haskell
```haskell
data Circle = C { x :: Double
                , y :: Double
                , r :: Double
                }
  deriving (Eq)
```

Rust には `Eq` と `PartialEq` の2つの Trait が存在する．

こいつらの持つ `==` には次のような違いがある．

- `Eq` : 同値関係

- `PartialEq` : 半同値関係（対称的で推移的だが，反射的ではない）

例えば Floating Point Number な型は `PartialEq` である．

`Circle` 型ではフィールドとして `f64` （64bit IEEE 浮動小数点数）を持たせているので，多分 `Eq` だとダメ．


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


## 配列

`Vec<T>` 型が使える．次のように定義できる．

```rust
let v = vec![0, 1, 2, 3, 4];
```

### 列長

- `len`: 長さ
- `capacity`: キャパシティ

```rust
let mut v = vec![0, 1, 2, 3, 4];
assert_eq!(v.len(), 5);
assert_eq!(v.capacity(), 5);
```

### 要素へのアクセス

- `[]`: 通常の index アクセス
- `get`: 安全な index アクセス（参照）

```rust
let v = vec![0, 1, 2, 3, 4];
assert_eq!(v[2], 2);

let g = v.get(2);
assert_eq!(g, Some(&2));
```

- `first`: 先頭の要素を安全に参照する
- `last`: 最後の要素を安全に参照する
- `pop`: 最後の要素を安全に取り出す

```
let fst = v.first();
assert_eq!(fst, Some(&0));

let lst = v.last();
assert_eq!(lst, Some(&4));

let mut v = vec![0, 1, 2, 3, 4];

let lst = v.pop();
assert_eq!(lst, Some(4));
assert_eq!(v, [0, 1, 2, 3]);
```

### 要素の追加

- `push`: 後方に要素を追加
- `insert`: 指定位置に要素を追加

```rust
let mut v = vec![0, 1, 2, 3];

v.push(4);
assert_eq!(v, [0, 1, 2, 4]);

v.insert(3, 3);
assert_eq!(v, [0, 1, 2, 3, 4]);
```

### 要素の削除

- `remove`: 指定位置の要素を削除

```rust
let mut v = vec![0, 1, 2, 3, 4];

let r = v.remove(2);
assert_eq!(r, 2);
assert_eq!(v, [0, 1, 3, 4]);
```

- `retain`: `filter` 的なもの

```rust
let mut v = vec![0, 1, 2, 3, 4];

v.retain(|&x| x < 3);
assert_eq!(v, [0, 1, 2]);
```

- `dedup`: 連続する重複要素を削除

```rust
let mut v = vec![0, 1, 2, 2, 1];

v.dedup();
assert_eq!(v, [0, 1, 2, 1]);

// ソートしとけばすべての重複要素を削除できる
v.sort();
v.dedup();
assert_eq!(v, [0, 1, 2]);
```

### その他の操作

- `swap`: スワップ
- `reverse`: 反転
- `sort`: ソート

```rust
let mut v = vec![0, 1, 2, 3, 4];

v.swap(2, 3);
assert_eq!(v, [0, 1, 3, 2, 4]);

v.reverse();
assert_eq!(v, [4, 2, 3, 1, 0]);

v.sort();
assert_eq!(v, [0, 1, 2, 3, 4]);
```

- `split_at`: 指定位置で2つに分割

```rust
let v = vec![0, 1, 2, 3, 4];

let (l, r) = v.split_at(3);
assert_eq!(l, [0, 1, 2]);
assert_eq!(r, [3, 4]);
```
