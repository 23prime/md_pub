## 参照 (reference) と借用 (borrowing)

- 参照は，所有権を持たないポインタ型．
- 値に対する参照を作ることを，借用という．

### 2種類の参照（借用）

|  種類  |共有参照 |可変参照|
|--------|---------|--------|
|  借用  |immutable|mutable |
|読み出し|   ◯    |   ◯   |
|  変更  |   ×    |   ◯   |
|複数参照|   ◯    |   ×   |
|  記法  | `&a`    |`&mut a`|

- 共有参照（immutable な借用）の例

  ```rust
  let a = 0;
  let b: &i32 = &a;  // 借用！
  assert_eq!(0, *b);
  ```

  ここでは `*b` で明示的に参照解決している．

  immutable な借用は，何回でも可能．

  ```rust
  let a = 0;
  let b: &i32 = &a;
  let c: &i32 = &a;
  assert_eq!(0, *b);
  assert_eq!(0, *c);
  ```

- 可変参照（mutable な借用）の例

  ```rust
  let mut a = 0;                  // 参照元の a も mutable である必要がある
  {
    let mut b: &mut i32 = &mut a; // 借用！
    *b += 1;                      // 参照解決して値を変更
    assert_eq!(1, *b);
  }                               // b がスコープから外れて a 
  assert_eq!(1, a);               // a の値も変更されていることがわかる
  ```

  mutable な借用は，原則1回のみ許される．次の2つのコードは通らない．

  ```rust
  let mut a = 0;
  let mut b: &mut i32 = &mut a;
  let mut c: &mut i32 = &mut a; // 2回目の借用をしようとすると怒られる
  ```

  ```rust
  let mut a = 0;
  let mut b: &mut i32 = &mut a;
  let c : &i32 = &a;            // immutable な借用と併用できない
  ```

  ただし，次のコードは通る．

  ```rust
  let mut a = 0;
  {
    let mut b: &mut i32 = &mut a;
    assert_eq!(0, *b);
  }
  let mut c: &mut i32 = &mut a;
  assert_eq!(0, *c);
  ```

### 参照の代入

```rust
let a = 0;
let b = 1;
let mut c = &a;
assert_eq!(0, *c);

c = &b;            // 参照 c へ参照を再代入
assert_eq!(1, *c);
assert_eq!(0, a); // 「借用しなおす」ため， a の値は変更されない
```

### ライフタイム (lifetime)

- ライフタイム：参照を（安全に）利用できる範囲

次のコードでは， `assert_eq!(*a, 1)` の段階で既に `b` がスコープから外れ， `a` の参照が `null` になってしまう．  
これを放っておくとダングリングポインタが生じるため，コンパイラに怒られる．

```rust
let mut a: &i32 = &0;
{
  let b = 0;
  a = &b;
}
assert_eq!(*a, 0);
```

↓ 実行結果．

```shell
error[E0597]: `b` does not live long enough
  --> src/main.rs:10:10
   |
10 |     a = &b;
   |          ^ borrowed value does not live long enough
11 |   }
   |   - `b` dropped here while still borrowed
12 |   assert_eq!(*a, 0);
13 | }
   | - borrowed value needs to live until here
```

ただし，次のコードは通る．

```rust
let mut a: &i32 = &0;
{
  let b = 0;
  let a = &b;
}
assert_eq!(*a, 0);
```