# Rust

Note for Rust compared with Haskell.

## Struct

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

## Trait
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

## Implementation

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

## Trait による型制約

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

## Derive

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