{-# LANGUAGE TypeNaturals, KindSignatures #-}
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


copyArray            :: (x :<= m, x :<= n, Storable a)
                     => Array m a -> Array n a -> Nat x -> IO ()
copyArray (MA to) (MA from) i = F.copyArray to from (natInt i)

moveArray            :: (x :<= m, x :<= n, Storable a)
                     => Array m a -> Array n a -> Nat x -> IO ()
moveArray (MA to) (MA from) i = F.moveArray to from (natInt i)



natInt               :: Nat n -> Int
natInt x              = fromIntegral (natToInteger x)


{-

reallocArray :: Storable a => Ptr a -> Int -> IO (Ptr a)
reallocArray0 :: Storable a => Ptr a -> Int -> IO (Ptr a)
peekArray0 :: (Storable a, Eq a) => a -> Ptr a -> IO [a]
pokeArray :: Storable a => Ptr a -> [a] -> IO ()
pokeArray0 :: Storable a => a -> Ptr a -> [a] -> IO ()
newArray :: Storable a => [a] -> IO (Ptr a)
newArray0 :: Storable a => a -> [a] -> IO (Ptr a)
withArray :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withArray0 :: Storable a => a -> [a] -> (Ptr a -> IO b) -> IO b
withArrayLen :: Storable a => [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen0 :: Storable a => a -> [a] -> (Int -> Ptr a -> IO b) -> IO b
lengthArray0 :: (Storable a, Eq a) => a -> Ptr a -> IO Int
advancePtr :: Storable a => Ptr a -> Int -> Ptr a

-- i.e. drop
advancePtr :: (Storable a) => Array (m + n) a -> Nat m -> Array n a
-}

