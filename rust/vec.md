## 配列

- 固定長配列： `[T; N]` （長さ `N` を型情報として持つ）
- 可変長配列： `Vec<T>`
- スライス： `&[T]`， `&mut [T]` （↑上の配列の領域（最初の要素のポインタと長さ）を指す）

多分 `Vec<T>` を一番使うので，以下それについて．

### 初期化

```rust
let v = vec![0, 1, 2, 3, 4];
```

または

```rust
let v = Vec::new();
```

または

```rust
let v: Vec<i32> = (0..4).collect();
```

### 列長

- `len`: 長さ
- `capacity`: 容量（確保されたメモリ長）

```rust
let mut v = vec![0, 1, 2, 3, 4];
assert_eq!(v.len(), 5);
assert_eq!(v.capacity(), 5);
```

### 要素へのアクセス

- `[]`: 通常の index アクセス
- `get`: 安全な index アクセス（`Option` 型で参照）

```rust
let v = vec![0, 1, 2, 3, 4];
assert_eq!(v[2], 2);

let g = v.get(2);
assert_eq!(g, Some(&2));
```

- `first`: 先頭の要素を安全に参照する
- `last`: 最後の要素を安全に参照する
- `pop`: 最後の要素を安全に取り出す

```rust
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

// ソートしておけばすべての重複要素を削除できる
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

### 配列に `map` する

- 直接はできないので `iter` を経由して `map` する．

```rust
let mut v = vec![0, 1, 2, 3, 4];

let v = v.iter().map(|&x| x * 2).collect::<Vec<i64>>();
assert_eq!(v, [0, 2, 4, 6, 8]);
```