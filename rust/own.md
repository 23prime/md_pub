## 所有権 (ownership) と移動 (move)

### 基本的なこと

- `String` 型 や `Vec<T>` 型等では，ポインタ・容量・長さを stack で，実際の値を heap で確保する．
- この heap で確保された値を所有 (own) しているという意味で，所有権 (ownership) という概念がある．
- なお，数値型や `char` 型等は，値を stack で確保する．

### 所有権

`String` 型で見てみる．

次のように変数宣言した段階で，heap 上に確保された値の所有権は， `a` にある．

```rust
let a = "hello".to_string();
```

### 所有権の移動

`b` に `a` の値を代入すると，値の所有権は `a` から `b` へ移動 (move) する．

```rust
let a = "hello".to_string();
let b = a;
assert_eq!("hello", b);
```

ここで，変数 `a` を使おうとすると，次のようにコンパイルエラー．

```rust
let a = "hello".to_string();
let b = a;
assert_eq!("hello", a);
```

```shell
error[E0382]: use of moved value: `a`
 --> src/main.rs:7:23
  |
6 |   let b = a;
  |       - value moved here
7 |   assert_eq!("hello", a);
  |                       ^ value used here after move
  |
  = note: move occurs because `a` has type `std::string::String`, which does not implement the `Copy` trait
```

### Clone

Rust ではデフォルトで上のように「値の所有権の移動」が行われるが，例外的にメモリ上に「値をコピー（クローン）」することもできる．

次のように書くことで， `a` の値とは別の領域に値のコピー（すなわち `b` の値）が作られる．

```rust
let a: String = "hello".to_string();
let b = a.clone();
assert_eq!("hello", a);
assert_eq!("hello", b);
```

### Copy Trait

`Copy` Trait を実装している型では，デフォルトで値はコピーされる．

例えば， `i32` 型は `Copy` Trait を実装しており，次のような記述をしてもエラーにならない．

```rust
let a: i32 = 0;
let b = a;
assert_eq!(0, b);
```