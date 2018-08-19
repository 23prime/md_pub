## グローバル変数

`static` で宣言できる．

```rust
static ZERO: i32 = 0;

#[test]
fn test_func() {
  assert_eq!(ZERO, 0);
}
```

mutable にもできるが， `unsafe` 内でしか使えない．

```rust
static mut ZERO: i32 = 0;

#[test]
fn test_func3() {
  unsafe {
    ZERO += 1;
    assert_eq!(ZERO, 1);
  }
}

#[test]
fn test_func4() {
  unsafe {
    assert_eq!(ZERO, 1);
  }
}
```

## ローカル変数

### 変数宣言

- immutable

```rust
let a = 0;
a += 1; // <- error
```

- mutable

```rust
let mut a = 0;
a += 1;
assert_eq!(a, 1);
```

### パターン

```rust
let (a, b) = (0, 1);
assert_eq!(a, 0);
assert_eq!(b, 1);

let [a, b] = [0, 1];
assert_eq!(a, 0);
assert_eq!(b, 1);
```

### 型注釈

```rust
let a: i32 = 0;
```

### 再宣言

- immutable な変数でもできる．
- 型が違ってもいい．

```rust
let a: i32 = 0;
let a: i64 = 1;
assert_eq!(a, 1);
```

### 再代入

- immutable な変数ではできない．

```rust
let mut a = 0;
a = 1;
assert_eq!(a, 1);
```

### ブロックスコープ

- 基本的に変数は `{}` 内で有効．

```rust
  {
    let a = 0;
    assert_eq!(a, 0);
  }
  assert_eq!(a, 0); // cannot find value `a` in this scope not found in this scope
```