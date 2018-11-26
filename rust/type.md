## 型システム

Haskell の型システムに似てる部分も多いので，実装例で比べてみる．

### データ型の定義

- `struct`

  直積型を書ける．

  Haskell では `data` とか `newtype` に相当する．

  - 円を表す型（構造体）

    ```rust
    // Rust
    struct Circle {
        x: f64,
        y: f64,
        r: f64,
    }
    ```

    値を取り出すには，

    ```rust
    let c = Circle {
        x: 1,
        y: 2,
        r: 3
    }
    
    c.x;
    ```

    などのようにする．

    ```haskell
    -- Haskell
    data Circle = Circle { x :: Double
                         , y :: Double
                         , r :: Double
                         }
    ```

  - 四元数

    ```rust
    // Rust
    struct Quat(f64, f64, f64, f64);
    ```

    ```haskell
    -- Haskell
    data Quat = Quat (Float, Float, Float, Float)
    ```

- `enum`

  直和型を書ける．

  これも `data` とか `newtype` に相当する．

  - 曜日

    ```rust
    // Rust
    enum Day {
        Sun, Mon, Tue, Wed, Thu, Fri, Sat
    }
    ```

    ```haskell
    -- Haskell
    data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
    ```

- `type`

  Haskell の `type` に相当する．

  ```rust
  // Rust
  type Mat = Vec<Vec<i64>>;
  ```

  ```haskell
  -- Haskell
  type Mat = [[Int]]
  ```

- その他

  制約付きの多相型も定義できる．

  上の `Mat` 型を，より一般的な数値型を受け取れるようにしてみる．

  ```rust
  // Rust
  use num::traits::Num;

  type Mat<T: Num> = Vec<Vec<T>>;
  ```

  これを Haskell で書こうとすると，次のようになる．

  ```haskell
  -- Haskell
  {-# LANGUAGE DatatypeContexts #-}

  newtype Num t => Mat t = Mat [[t]]
  ```

  ちなみに `DatatypeContexts` は非推奨な古い機能を提供する拡張なので，今は `GADTs` で書くのが良さそう．

  ```haskell
  -- Haskell
  {-# LANGUAGE GADTs #-}

  data Mat t where
    Mat :: Num t => [[t]] -> Mat t
  ```

### Trait

Haskell の型クラスに相当する．

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

Haskell の instance に相当する．

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

- 円の面積を表示する関数．

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
data Circle = Circle { x :: Double
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

`Circle` 型ではフィールドとして `f64` （64bit IEEE 浮動小数点数）を持たせているので， `Eq` だとダメ．