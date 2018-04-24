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