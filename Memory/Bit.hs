{-# LANGUAGE TypeNaturals #-}
module Memory.Bit
  ( Bit
  , natBitSize
  , toList
  , showBin, showHex
  , split, cat
  ) where

import GHC.TypeNats (Nat, TypeNat(..), natToInteger, type (+))
import Data.Bits (Bits(..))
import qualified Numeric as N

newtype Bit (n :: Nat) = B Integer

rep :: Bit n -> Integer
rep (B x) = x

natBitSize :: TypeNat n => Bit n -> Nat n
natBitSize _ = nat

numBitSize :: (TypeNat n, Num a) => Bit n -> a
numBitSize = fromIntegral . natToInteger . natBitSize

norm :: TypeNat n => Bit n -> Bit n
norm b@(B n) = B (n .&. ((1 `shiftL` numBitSize b) - 1))


instance Show (Bit n) where
  showsPrec p = showsPrec p . rep

instance TypeNat n => Read (Bit n) where
  readsPrec p txt = [ (fromInteger x, cs) | (x,cs) <- readsPrec p txt ]

instance Eq (Bit n) where
  B x == B y          = x == y

instance Ord (Bit n) where
  compare (B x) (B y) = compare x y

instance TypeNat n => Bounded (Bit n) where
  minBound            = B 0
  maxBound            = norm $ B (-1)

instance TypeNat n => Num (Bit n) where
  B x + B y           = norm $ B (x + y)
  B x - B y           = norm $ B (x - y)
  B x * B y           = norm $ B (x * y)
  negate (B x)        = norm $ B $ negate x
  abs x               = x
  signum (B x)        = B (signum x)
  fromInteger x       = norm $ B x

instance TypeNat n => Bits (Bit n) where
  isSigned _          = False
  bitSize             = numBitSize
  B x .&. B y         = B (x .&. y)
  B x .|. B y         = B (x .|. y)
  xor (B x) (B y)     = B (xor x y)
  complement (B x)    = norm $ B (complement x)
  shiftR (B x) n      = B (shiftR x n)
  shiftL (B x) n      = norm $ B (shiftL x n)
  testBit (B x) n     = testBit x n
  rotate b@(B x) n'
    | sz < 2          = b
    | otherwise       = norm (B (shiftL x n)) .|. norm (B (shiftR x (sz-n)))
    where
    sz = bitSize b
    n  = mod n' sz

toList :: TypeNat n => Bit n -> [Bool]
toList b = map (testBit b) [ start, start - 1 .. 0 ]
  where start = numBitSize b - 1 :: Int

showBin :: TypeNat n => Bit n -> String
showBin = map sh . toList
  where sh x = if x then '1' else '0'

showHex :: TypeNat n => Bit n -> String
showHex b = zeros (N.showHex (rep b) "")
  where zeros n = replicate (len - length n) '0' ++ n
        len     = div (bitSize b + 3) 4

split :: TypeNat n => Bit (m + n) -> (Bit m, Bit n)
split (B x) = (a, b)
  where a = B (x `shiftR` bitSize b)
        b = norm (B x)

cat :: TypeNat n => Bit m -> Bit n -> Bit (m + n)
cat (B x) b@(B y) = B (shiftL x (numBitSize b) .|. y)




