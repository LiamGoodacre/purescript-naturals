module Data.Natural
  ( Natural()
  , intToNat
  , natToInt
  , minus
  , (+-)
  )
  where

import Prelude

import Data.Enum (class Enum)
import Data.Maybe (Maybe(Just, Nothing))

newtype Natural =
  UnsafeMakeNatural Int

-- | Construct a natural number from an integer.
-- | If the integer is less than 0, then the result will be 0 (nat)
intToNat :: Int -> Natural
intToNat i | i >= 0 = UnsafeMakeNatural i
intToNat _          = UnsafeMakeNatural 0

-- | Convert a natural number back to an integer
natToInt :: Natural -> Int
natToInt (UnsafeMakeNatural i) = i

unaryViaInt :: (Int -> Int) -> Natural -> Natural
unaryViaInt f x = intToNat (f (natToInt x))

binaryViaInt :: (Int -> Int -> Int) -> Natural -> Natural -> Natural
binaryViaInt f l r = intToNat (f (natToInt l) (natToInt r))

instance eqNatural :: Eq Natural where
  eq l r = natToInt l == natToInt r

instance ordNatural :: Ord Natural where
  compare l r = natToInt l `compare` natToInt r

instance enumNatural :: Enum Natural where
  succ :: Natural -> Maybe Natural
  succ n = Just (n + one)
  pred :: Natural -> Maybe Natural
  pred n = if n == zero then Nothing else Just (n +- one)

instance showNatural :: Show Natural where
  show = show <<< natToInt

instance semiringNatural :: Semiring Natural where
  one = intToNat 1
  mul = binaryViaInt (*)
  zero = intToNat 0
  add = binaryViaInt (+)

-- | Subtract a natural number from another.
-- | When `l < r` then `l +- r = 0`.
-- | This is provided due to Natural not being a Ring as it violates the
-- | additive inverse law: `a - a = (zero - a) + a = zero`
minus :: Natural -> Natural -> Natural
minus = binaryViaInt (-)

infixl 6 minus as +-

