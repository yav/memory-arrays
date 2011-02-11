{-# LANGUAGE TypeNaturals #-}
module Data.Bit
  ( Bit
  , natBitSize, numBitSize
  , toList
  , showBin, showHex
  , split, (#)
  , coerceBit
  ) where

import GHC.TypeNats (Nat, NatI(..), natToInteger, type (+), checkNat)
import Data.Bits (Bits(..))
import qualified Numeric as N
import Control.DeepSeq(NFData(..))

newtype Bit (n :: Nat) = B Integer

natBitSize :: NatI n => Bit n -> Nat n
natBitSize _ = nat

numBitSize :: (NatI n, Num a) => Bit n -> a
numBitSize = fromIntegral . natToInteger . natBitSize

coerceBit :: (NatI a, NatI b) => Bit a -> Maybe (Bit b)
coerceBit x@(B n) =
  do a <- checkNat (== numBitSize x)
     return (B n `ofWidth` a)
  where ofWidth :: Bit n -> Nat n -> Bit n
        ofWidth a _ = a


instance Show (Bit n) where
  showsPrec p (B x) = showsPrec p x

instance NatI n => Read (Bit n) where
  readsPrec p txt = [ (fromInteger x, cs) | (x,cs) <- readsPrec p txt ]

instance Eq (Bit n) where
  B x == B y          = x == y

instance Ord (Bit n) where
  compare (B x) (B y) = compare x y

instance NatI n => Bounded (Bit n) where
  minBound            = B 0
  maxBound            = norm $ B (-1)

instance NatI n => Num (Bit n) where
  B x + B y           = norm $ B (x + y)
  B x - B y           = norm $ B (x - y)
  B x * B y           = norm $ B (x * y)
  negate (B x)        = norm $ B $ negate x
  abs x               = x
  signum (B x)        = B (signum x)
  fromInteger x       = norm $ B x

instance NatI n => Bits (Bit n) where
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

toList :: NatI n => Bit n -> [Bool]
toList b = map (testBit b) [ start, start - 1 .. 0 ]
  where start = numBitSize b - 1 :: Int

showBin :: NatI n => Bit n -> String
showBin = map sh . toList
  where sh x = if x then '1' else '0'

showHex :: NatI n => Bit n -> String
showHex b@(B x) = zeros (N.showHex x "")
  where zeros n = replicate (len - length n) '0' ++ n
        len     = div (bitSize b + 3) 4

split :: NatI n => Bit (m + n) -> (Bit m, Bit n)
split (B x) = (a, b)
  where a = B (x `shiftR` bitSize b)
        b = norm (B x)

(#) :: NatI n => Bit m -> Bit n -> Bit (m + n)
B x # b@(B y) = B (shiftL x (numBitSize b) .|. y)

instance NatI n => Real (Bit n) where
  toRational (B x) = toRational x

instance NatI n => Enum (Bit n) where
  toEnum x        = norm $ B $ toEnum x
  fromEnum (B x)  = fromEnum x    -- wraps around
  succ x          = x + 1
  pred x          = if x == minBound then maxBound else x - 1
  enumFrom x      = enumFromTo x maxBound
  enumFromTo x y
    | x < y       = enumFromThenTo x (succ x) y
    | x == y      = [x]
    | otherwise   = []

  enumFromThen x y = enumFromThenTo x y bound
      where
        bound | x <= y    = maxBound
              | otherwise = minBound

  enumFromThenTo (B x) (B y) (B z) = map B (enumFromThenTo x y z)

instance NatI n => Integral (Bit n) where
  toInteger (B x) = x
  quotRem (B x) (B y) = let (a,b) = quotRem x y
                        in (B a, B b)

instance NFData (Bit n) where
  rnf (B x) = rnf x

norm :: NatI n => Bit n -> Bit n
norm b@(B n) = B (n .&. ((1 `shiftL` numBitSize b) - 1))

