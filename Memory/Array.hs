{-# LANGUAGE TypeNaturals, KindSignatures #-}
{-# LANGUAGE Rank2Types, ExistentialQuantification #-}
module Memory.Array
  ( Array
  , foreignArray
  , mallocArray
  , mallocArray0
  , allocaArray
  , allocaArray0
  , peekAt
  , pokeAt
  , peekArray
  , copyArray
  , moveArray
  , module Memory.Ix
  ) where

import GHC.TypeNats
import Memory.Ix

import Foreign.Ptr
import Foreign.Storable
import qualified Foreign.Marshal.Array as F


newtype Array (n :: Nat) a = MA (Ptr a)

arraySize            :: TypeNat n => Array n a -> Nat n
arraySize _           = nat

foreignArray         :: Ptr a -> Array n a
foreignArray          = MA

peekAt               :: Storable a => Array n a -> Ix n -> IO a
peekAt (MA p) i       = peekElemOff p (fromIx i)

pokeAt               :: Storable a => Array n a -> Ix n -> a -> IO ()
pokeAt (MA p) i x     = pokeElemOff p (fromIx i) x

mallocArray          :: Storable a => Nat n -> IO (Array n a)
mallocArray n         = MA `fmap` F.mallocArray (natInt n)

mallocArray0         :: Storable a => Nat n -> IO (Array n a)
mallocArray0 n        = MA `fmap` F.mallocArray0 (natInt n)

allocaArray          :: Storable a => Nat n -> (Array n a -> IO b) -> IO b
allocaArray n k       = F.allocaArray (natInt n) (k . MA)

allocaArray0         :: Storable a => Nat n -> (Array n a -> IO b) -> IO b
allocaArray0 n k      = F.allocaArray0 (natInt n) (k . MA)

peekArray            :: (TypeNat n, Storable a) => Array n a -> IO [a]
peekArray             = mk nat
  where mk :: Storable a => Nat n -> Array n a -> IO [a]
        mk n (MA p)   = F.peekArray (natInt n) p

pokeArray            :: (TypeNat n, Storable a) => Array n a -> [a] -> IO ()
pokeArray a@(MA p) xs   = F.pokeArray p (take (natInt (arraySize a)) xs)



copyArray            :: (x :<= m, x :<= n, Storable a)
                     => Array m a -> Array n a -> Nat x -> IO ()
copyArray (MA to) (MA from) i = F.copyArray to from (natInt i)

moveArray            :: (x :<= m, x :<= n, Storable a)
                     => Array m a -> Array n a -> Nat x -> IO ()
moveArray (MA to) (MA from) i = F.moveArray to from (natInt i)


withArray            :: Storable a
                     => [a] -> (forall n. Array n a -> IO b) -> IO b
withArray as k        = F.withArray as (\p -> k (MA p))

withArrayLen         :: Storable a
                     => [a] -> (forall n. Nat n -> Array n a -> IO b) -> IO b
withArrayLen as k     = F.withArrayLen as (\n p ->
                          intNat n (\n1 -> k n1 (MA p))) 

--------------------------------------------------------------------------------
-- Array pointers packaged with the array size.

data SizedArray a      = forall n. SA (Nat n) (Array n a)

sizedArraySize       :: SizedArray n -> Integer
sizedArraySize (SA n _) = natToInteger n

-- XXX: It's a pity that newArray does not give us the length of the array.
-- It must have compute it to allocate the array in the first place.
newArray             :: Storable a => [a] -> IO (SizedArray a)
newArray xs           = do p <- F.newArray xs
                           return (intNat (length xs) (\n -> SA n (MA p)))



arrayToSizedArray    :: TypeNat n => Array n a -> SizedArray a
arrayToSizedArray a   = SA (arraySize a) a

sizedArrayToArray    :: SizedArray a -> (forall n. Nat n -> Array n a -> b) -> b
sizedArrayToArray (SA s a) k = k s a


--------------------------------------------------------------------------------

natInt               :: Nat n -> Int
natInt x              = fromIntegral (natToInteger x)

intNat               :: Int -> (forall n. Nat n -> a) -> a
intNat x k            = integerToNat (toEnum x) k


{-

reallocArray :: Storable a => Ptr a -> Int -> IO (Ptr a)
reallocArray0 :: Storable a => Ptr a -> Int -> IO (Ptr a)
peekArray0 :: (Storable a, Eq a) => a -> Ptr a -> IO [a]
pokeArray0 :: Storable a => a -> Ptr a -> [a] -> IO ()
newArray0 :: Storable a => a -> [a] -> IO (Ptr a)
withArray0 :: Storable a => a -> [a] -> (Ptr a -> IO b) -> IO b
withArrayLen0 :: Storable a => a -> [a] -> (Int -> Ptr a -> IO b) -> IO b
lengthArray0 :: (Storable a, Eq a) => a -> Ptr a -> IO Int
advancePtr :: Storable a => Ptr a -> Int -> Ptr a

-- i.e. drop
advancePtr :: (Storable a) => Array (m + n) a -> Nat m -> Array n a
-}
