{-# LANGUAGE TypeNaturals #-}
module Data.Bit
  ( Bit
  , natBitSize, numBitSize
  , toList
  , showBin, showHex
  , split, (#)
  , coerce
  , bitFromInteger
  , bitToInteger
  , bitAdd
  , bitAdd'
  , bitSub
  , bitSub'
  , bitMul
  , bitMul'
  , bitQuotRem
  , bitNeg
  , bitReadsPrec
  , bitMinBound
  , bitMaxBound
  , bitSigNum
  , bitAnd
  , bitOr
  , bitXor
  , bitComplement
  , bitSplit
  , bitJoin
  , bitCoerce
  , bitShiftR
  , bitShiftL
  , bitTestBit
  , bitRotate
  , bitToList
  , bitShowBin
  , bitShowHex
  ) where

import GHC.TypeNats (Nat, NatI(..), natToInteger, type (+))
import Data.Bits (Bits(..))
import qualified Numeric as N
import Control.Monad(guard)
import Control.DeepSeq(NFData(..))

newtype Bit (n :: Nat) = B Integer

bitFromInteger :: Nat n -> Integer -> Bit n
bitFromInteger w x = norm w (B x)

bitToInteger :: Nat n -> Bit n -> Integer
bitToInteger _ (B x) = x

bitAdd :: Nat n -> Bit n -> Bit n -> Bit n
bitAdd = lift2 (+)

bitAdd' :: Nat n -> Bit n -> Bit n -> Bit (n + 1)
bitAdd' _ (B x) (B y) = B (x + y)

bitSub :: Nat n -> Bit n -> Bit n -> Bit n
bitSub = lift2 (-)

bitSub' :: Nat n -> Bit n -> Bit n -> Bit (n + 1)
bitSub' _ (B x) (B y) = B (x - y)

bitMul :: Nat n -> Bit n -> Bit n -> Bit n
bitMul = lift2 (*)

bitMul' :: Nat n -> Bit n -> Bit n -> Bit (m + n)
bitMul' _ (B x) (B y) = B (x * y)

bitQuotRem :: Nat n -> Bit n -> Bit n -> (Bit n, Bit n)
bitQuotRem _ (B x) (B y) = let (a,b) = quotRem x y in (B a, B b)  -- XXX: ok?

bitNeg :: Nat n -> Bit n -> Bit n
bitNeg = lift1 negate

bitReadsPrec :: Nat n -> Int -> ReadS (Bit n)
bitReadsPrec w p txt = [ (bitFromInteger w x, cs) | (x,cs) <- readsPrec p txt ]

bitMinBound :: Nat n -> Bit n
bitMinBound _ = B 0

bitMaxBound :: Nat n -> Bit n
bitMaxBound w = norm w (B (-1))

bitSigNum :: Nat n -> Bit n -> Bit n
bitSigNum _ (B x) = B (signum x)

bitAnd :: Nat n -> Bit n -> Bit n -> Bit n
bitAnd _ (B x) (B y)  = B (x .&. y)

bitOr :: Nat n -> Bit n -> Bit n -> Bit n
bitOr _ (B x) (B y)  = B (x .|. y)

bitXor :: Nat n -> Bit n -> Bit n -> Bit n
bitXor _ (B x) (B y)  = B (x .|. y)

bitComplement :: Nat n -> Bit n -> Bit n
bitComplement = lift1 complement

bitSplit :: Nat n -> Bit (m + n) -> (Bit m, Bit n)
bitSplit n (B x) = (a, b)
  where a = B (x `shiftR` ni n)
        b = bitFromInteger n x

bitJoin :: Nat n -> Bit m -> Bit n -> Bit (m + n)
bitJoin n (B x) (B y) = B (shiftL x (ni n) .|. y)

bitCoerce :: Nat m -> Nat n -> Bit m -> Maybe (Bit n)
bitCoerce x y (B m) = guard ((ni x :: Integer) == ni y) >> return (B m)

bitShiftR :: Nat n -> Bit n -> Int -> Bit n
bitShiftR _ (B x) n = B (shiftR x n)

bitShiftL :: Nat n -> Bit n -> Int -> Bit n
bitShiftL w x n = lift1 (`shiftL` n) w x

bitTestBit :: Nat n -> Bit n -> Int -> Bool
bitTestBit _ (B x) n  = testBit x n

bitRotate :: Nat n -> Bit n -> Int -> Bit n
bitRotate sz b@(B x) n'
  | si < 2    = b
  | otherwise = bitOr sz (bitFromInteger sz (shiftL x n))
                         (bitFromInteger sz (shiftR x (si - n)))
  where n = mod n' si
        si = ni sz


bitToList :: Nat n -> Bit n -> [Bool]
bitToList n b = map (bitTestBit n b) [ start, start - 1 .. 0 ]
  where start = ni n - 1 :: Int

bitShowBin :: Nat n -> Bit n -> String
bitShowBin n = map sh . bitToList n
  where sh x = if x then '1' else '0'

bitShowHex :: Nat n -> Bit n -> String
bitShowHex w (B x) = zeros (N.showHex x "")
  where zeros n = replicate (len - length n) '0' ++ n
        len     = div (ni w + 3) 4

--------------------------------------------------------------------------------

natBitSize :: NatI n => Bit n -> Nat n
natBitSize _ = nat

numBitSize :: (NatI n, Num a) => Bit n -> a
numBitSize = ni . natBitSize

coerce :: (NatI a, NatI b) => Bit a -> Maybe (Bit b)
coerce = bitCoerce nat nat

toList :: NatI n => Bit n -> [Bool]
toList = bitToList nat

showBin :: NatI n => Bit n -> String
showBin = bitShowBin nat

showHex :: NatI n => Bit n -> String
showHex = bitShowHex nat

split :: NatI n => Bit (m + n) -> (Bit m, Bit n)
split = bitSplit nat

(#) :: NatI n => Bit m -> Bit n -> Bit (m + n)
(#) = bitJoin nat




instance Show (Bit n) where
  showsPrec p (B x) = showsPrec p x

instance NatI n => Read (Bit n) where
  readsPrec = bitReadsPrec nat

instance Eq (Bit n) where
  B x == B y          = x == y

instance NFData (Bit n) where
  rnf (B x) = seq x ()

instance Ord (Bit n) where
  compare (B x) (B y) = compare x y

instance NatI n => Bounded (Bit n) where
  minBound = bitMinBound nat
  maxBound = bitMaxBound nat

instance NatI n => Num (Bit n) where
  (+)           = bitAdd nat
  (-)           = bitSub nat
  (*)           = bitMul nat
  negate        = bitNeg nat
  abs x         = x
  signum        = bitSigNum nat
  fromInteger   = bitFromInteger nat

instance NatI n => Bits (Bit n) where
  isSigned _    = False
  bitSize       = numBitSize
  (.&.)         = bitAnd nat
  (.|.)         = bitOr nat
  xor           = bitXor nat
  complement    = bitComplement nat
  shiftR        = bitShiftR nat
  shiftL        = bitShiftL nat
  testBit       = bitTestBit nat
  rotate        = bitRotate nat

instance NatI n => Real (Bit n) where
  toRational (B x) = toRational x

instance NatI n => Enum (Bit n) where
  toEnum x        = norm nat $ B $ toEnum x
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
  toInteger = bitToInteger nat
  quotRem   = bitQuotRem nat

norm :: Nat n -> Bit n -> Bit n
norm w (B n) = B (n .&. ((1 `shiftL` ni w) - 1))

ni :: Num a => Nat n -> a
ni = fromIntegral . natToInteger

lift1 :: (Integer -> Integer) -> Nat n -> Bit n -> Bit n
lift1 f w (B x) = norm w (B (f x))

lift2 :: (Integer -> Integer -> Integer) -> Nat n -> Bit n -> Bit n -> Bit n
lift2 f w (B x) (B y) = norm w (B (f x y))


