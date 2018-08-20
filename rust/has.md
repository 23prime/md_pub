## Compared with Haskell.

### Struct

- 円を表す型（構造体）

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

- 四元数

  Rust

  ```rust
  struct Quat(f64, f64, f64, f64);
  ```

  Haskell

  ```haskell
  data Quat = Quat (Float, Float, Float, Float)
  ```

  - 曜日

  Rust

  ```rust
  enum Day {
     Sun, Mon, Tue, Wed, Thu, Fri, Sat
  }
  ```

  Haskell

  ```haskell
  data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
  ```

ジェネリックな型も定義できる．

```rust
enum Hoge<T: Num> {
  Fuga(T)
}
```

これをそのまま Haskell で書くと，次のようになる．

```haskell
{-# LANGUAGE DatatypeContexts #-}

data Num a => Hoge a = Fuga a
```

しかし， `DatatypeContexts` は非推奨な古い機能を提供する拡張なので，今は `GADTs` で書くのが良さそう．

```haskell
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

data Hoge :: * -> * where
  Fuga :: Num a => a -> Hoge a
```

### Trait

Haskell の type class に相当する．

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