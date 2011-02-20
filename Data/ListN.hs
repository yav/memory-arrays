{-# LANGUAGE TypeNaturals, GADTs #-}
module Data.ListN where

import GHC.TypeNats

--------------------------------------------------------------------------------

data List :: Nat -> * -> * where
  Nil  :: List 0 a
  (:>) :: a -> List n a -> List (n + 1) a

toList :: List n a -> [a]
toList Nil        = []
toList (x :> xs)  = x : toList xs

fromList :: Nat n -> [a] -> List n a
fromList n xs = def (natToNatU n) xs
  where
  def :: NatU n -> [a] -> List n a
  def Zero _             = Nil
  def (Succ n) ~(a:as)   = a :> def n as

fromListI :: NatI n => [a] -> List n a
fromListI = fromList nat

instance Show a => Show (List n a) where
  show = show . toList

join2 :: List m a -> List n a -> List (m + n) a
join2 Nil xs        = xs
join2 (x :> xs) ys  = x :> join2 xs ys

joinMany :: List m (List n a) -> List (m * n) a
joinMany Nil          = Nil
joinMany (xs :> yss)  = join2 xs (joinMany yss)

repl :: Nat n -> a -> List n a
repl x a = def (natToNatU x) a
  where
  def :: NatU n -> a -> List n a
  def Zero _      = Nil
  def (Succ m) a  = a :> def m a

replI :: NatI n => a -> List n a
replI = repl nat


split2 :: Nat m -> List (m + n) a -> (List m a, List n a)
split2 = def . natToNatU
  where
  def :: NatU m -> List (m + n) a -> (List m a, List n a)
  def Zero xs             = (Nil, xs)
  def (Succ n) (x :> xs)  = let (as,bs) = def n xs in (x :> as, bs)

split2I :: NatI m => List (m + n) a -> (List m a, List n a)
split2I = split2 nat

splitMany :: Nat m -> Nat n -> List (m * n) a -> List m (List n a)
splitMany m n xs = def (natToNatU m) n xs
  where
  def :: NatU m -> Nat n -> List (m * n) a -> List m (List n a)
  def Zero _ _      = Nil
  def (Succ m) n xs = let (as,bs) = split2 n xs
                      in as :> def m n bs

splitManyI :: (NatI m, NatI n) => List (m * n) a -> List m (List n a)
splitManyI xs = splitMany nat nat xs


example :: List 10 (List 3 Int)
example = splitManyI (fromListI [0..])

