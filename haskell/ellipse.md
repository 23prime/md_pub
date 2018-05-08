# Ellipse

楕円曲線上の有理点演算を書いてみたやつ．

```haskell
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DatatypeContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ellipse where

import           Data.Proxy
import           GHC.TypeLits

import           Field

-------------
-- Samples --
-------------
-- y^2 = x^3 + x + 1 in F_5
p0 = E (0, 1) :: Ellipse (F 5) (Fr 1 1)
p1 = E (0, 4) :: Ellipse (F 5) (Fr 1 1)
p2 = E (2, 1) :: Ellipse (F 5) (Fr 1 1)
p3 = E (2, 4) :: Ellipse (F 5) (Fr 1 1)
p4 = E (3, 1) :: Ellipse (F 5) (Fr 1 1)
p5 = E (3, 4) :: Ellipse (F 5) (Fr 1 1)
p6 = E (4, 2) :: Ellipse (F 5) (Fr 1 1)
p7 = E (4, 3) :: Ellipse (F 5) (Fr 1 1)
ps = [Infty, p0, p1, p2, p3, p4, p5, p6, p7]

-- y^2 = x^3 + 4 x in Q
q0 = E (0, 0)  :: Ellipse Rational (Fr 4 1)
q1 = E (-2, 0) :: Ellipse Rational (Fr 4 1)
q2 = E (2, 0)  :: Ellipse Rational (Fr 4 1)
qs = [Infty, q0, q1, q2]

-----------------
-- Define Frac --
-----------------
data Frac = Fr Nat Nat

data SFrac (n :: Frac) = SFrac
  {-#UNPACK#-}!Int
  {-#UNPACK#-}!Int

class KnownFrac (n :: Frac) where
  fracSing :: SFrac n

instance (KnownNat a, KnownNat b) => KnownFrac (Fr a b) where
  fracSing = SFrac
    (fromIntegral $ natVal (Proxy::Proxy a))
    (fromIntegral $ natVal (Proxy::Proxy b))

fracVal :: forall f n proxy. (KnownFrac n, Fractional f) => proxy n -> f
fracVal _ = case fracSing :: SFrac n of
              SFrac a b -> fromIntegral a / fromIntegral b


--------------------
-- Define Ellipse --
--------------------
data Ellipse a (p :: Frac) = E (a, a) | Infty deriving (Eq, Ord)

instance (KnownFrac p, Fractional a, Show a) => Show (Ellipse a p) where
  show (E (x, y)) = show (x, y)
  show Infty      = "Intfy"

instance (KnownFrac p, Eq a, Fractional a) => Num (Ellipse a p) where
  (+)                     = addEllipse
  E (x1, y1) - E (x2, y2) = E (x1, y1) + (- E (x2, y2))
  (*)                     = undefined
  negate (E (x, y))       = E (x, - y) -- only (F p)
  abs                     = id
  signum                  = undefined
  fromInteger             = undefined


---------------------------
-- Functions for Ellipse --
---------------------------

-- Make a in "y^2 = x^3 + a x + b"
mkCoeff :: (KnownFrac p, Eq a, Fractional a) => Ellipse a p -> a
mkCoeff = fracVal

-- Addition of Ellipse
addEllipse :: (KnownFrac p, Eq a, Fractional a) => Ellipse a p -> Ellipse a p -> Ellipse a p
addEllipse x Infty = x
addEllipse Infty y = y
addEllipse e@(E (x1, y1)) (E (x2, y2))
  | x1 == x2 && (y1 /= y2 || (y1, y2) == (0, 0)) = Infty
  | otherwise                                    = E (x, y)
  where
    diffs = (x2 - x1, y2 - y1)
    s     = case diffs of
              (0, 0) -> (3 * x1 ^ 2 + mkCoeff e) / (2 * y1)
              _      -> snd diffs / fst diffs
    x     = s ^ 2 - x1 - x2
    y     = s * (x1 - x) - y1

-- Scalar multiplication of Ellipse
scalarMul :: (KnownFrac p, Eq a, Fractional a) => Int -> Ellipse a p -> Ellipse a p
scalarMul n = foldl (+) Infty . replicate n

-- Generate [Point] with Addition from a Point in Ellipse
generateEs :: (KnownFrac p, Eq a, Fractional a) => Ellipse a p -> [Ellipse a p]
generateEs Infty = [Infty]
generateEs e     = make e e []
  where
    make e0 e es
      | e `elem` es = es
      | otherwise   = make e0 (e + e0) $ es ++ [e]

-- Like fmap
emap :: (Fractional a, Fractional b) => (a -> b) -> Ellipse a p -> Ellipse b p
emap f (E (x, y)) = E (f x, f y)
```