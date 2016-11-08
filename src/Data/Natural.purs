module Data.Natural
( Natural()
, intToNat
, natToInt
, minus
, (+-)
)
where

import Prelude

newtype Natural =
  UnsafeMakeNatural Int

intToNat :: Int -> Natural
intToNat i | i >= 0 = UnsafeMakeNatural i
intToNat _          = UnsafeMakeNatural 0

natToInt :: Natural -> Int
natToInt (UnsafeMakeNatural i) = i

unaryViaInt :: (Int -> Int) -> Natural -> Natural
unaryViaInt f x = intToNat (f (natToInt x))

binaryViaInt :: (Int -> Int -> Int) -> Natural -> Natural -> Natural
binaryViaInt f l r = intToNat (f (natToInt l) (natToInt r))

instance eqNatural :: Eq Natural where
  eq l r = natToInt l == natToInt r

instance showNatural :: Show Natural where
  show = show <<< natToInt

instance semiringNatural :: Semiring Natural where
  one = intToNat 1
  mul = binaryViaInt (*)
  zero = intToNat 0
  add = binaryViaInt (+)

-- not a Ring as it violates 'additive inverse' law:
-- `a - a = (zero - a) + a = zero`

minus :: Natural -> Natural -> Natural
minus = binaryViaInt (-)

infixl 6 minus as +-

