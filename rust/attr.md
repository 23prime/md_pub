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

他に，`unused_imports` `unused_variables` 等を無視したい時に使える．

※ allow の逆は warn