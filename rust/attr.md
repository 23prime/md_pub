## Aattributes

### allow

```rust
#[allow(dead_code)]
fn hoge(...) {...}
```

で，コンパイル時に `hoge` が unused でも無視する．また，

```rust
#![allow(dead_code)]
```

を先頭に書けば，すべての関数に対して unused を無視する．

他に，

- `unused_imports`
- `unused_variables`
- `unused_assignments`

等が使える．

※ `#[allow]` の逆は`#[warn]`


### test

`assert_eq!` 等を用いて，簡単なテストを行う．

```rust
#[test]
fn test_func() {
  let a = 0;
  assert_eq!(a, 0);
}
```