{-# LANGUAGE TypeNaturals, KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Memory.Ix
  ( Ix, IxRep, MaxIx, IxNat
  , fromIx, (<=?), ixMod, cast
  ) where

import GHC.TypeNats
import Data.Word(Word)

-- | The underlying type used to represent indexes.
type IxRep            = Word

-- | The largest supported index type.
-- Currently, this is limited to the largest "Int",
-- because the "peek" and "poke" operations use "Int" for indexing.
type MaxIx            = 9223372036854775808



-- | Numbers in the interval [0,n).
newtype Ix (n :: Nat) = Ix IxRep deriving (Eq,Ord)
-- Note: We keep indexes "normalized".

-- | Convert an index into its underlying representation type.
fromIx :: Num a => Ix n -> a
fromIx (Ix n) = fromIntegral n

-- | Convert a number into an index, as long as the number is less then,
-- or equal to the second argument.
(<=?) :: IxRep -> Ix n -> Maybe (Ix n)
x <=? Ix y  = if x <= y then Just (Ix x) else Nothing

cast :: (m :<= n) => Ix m -> Ix n
cast (Ix m) = Ix m


-- | Type level naturals for non-empty Ix types.
class    (TypeNat n, 1 :<= n, n :<= MaxIx) => IxNat n
instance (TypeNat n, 1 :<= n, n :<= MaxIx) => IxNat n

-- private
natRep :: Nat n -> IxRep
natRep n  = fromInteger (natToInteger n)

instance IxNat n => Bounded (Ix n) where
  minBound    = Ix 0
  maxBound    = mk nat
    where mk :: Nat n -> Ix n
          mk n = Ix (natRep n)

instance Show (Ix n) where
  showsPrec p (Ix n)  = showsPrec p n

instance IxNat n => Read (Ix n) where
  readsPrec p s       = do (x,s1) <- readsPrec p s
                           case x <=? maxBound of
                             Just n  -> return (n,s1)
                             Nothing -> []


-- | Convert a number into an index, using modulo arithmetic.
ixMod :: IxNat n => IxRep -> Ix n
ixMod x = modulo nat
  where modulo :: Nat n -> Ix n
        modulo m  = Ix (mod x (natRep m))

instance IxNat n => Num (Ix n) where
  Ix x + Ix y   = ixMod (x + y)
  Ix x * Ix y   = ixMod (x * y)
  abs x         = x
  signum (Ix x) = Ix (signum x)
  fromInteger i = mk nat
    where mk :: Nat n -> Ix n
          mk n  = Ix (fromInteger (mod i (natToInteger n)))

  negate i | i == 0 = Ix 0
  negate (Ix x)     = mk nat
    where mk :: Nat n -> Ix n
          mk n      = Ix (natRep n - x)


instance IxNat n => Enum (Ix n) where
  fromEnum (Ix n) = fromEnum n
  toEnum i        = mk nat
    where mk :: Nat n -> Ix n
          mk n    = Ix (toEnum (mod i (fromInteger (natToInteger n))))




