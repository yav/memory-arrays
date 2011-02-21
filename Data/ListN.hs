{-# LANGUAGE TypeNaturals, GADTs, FlexibleContexts #-}
module Data.ListN where

import GHC.TypeNats

--------------------------------------------------------------------------------

class Listable list where
  toList :: list a -> [a]

infixr :-
data Stream a       = a :- Stream a

unfold :: (b -> (a,b)) -> b -> Stream a
unfold f b = let (a,b') = f b in a :- unfold f b'

instance Listable Stream where
  toList (a :- as)  = a : toList as

instance Functor Stream where
  fmap f = unfold (\(a :- as) -> (f a, as))

instance Show a => Show (Stream a) where
  show = show . toList

splitStream :: Nat n -> Stream a -> (List n a, Stream a)
splitStream n xs = def (natToNatU n) xs
  where
  def :: NatU n -> Stream a -> (List n a, Stream a)
  def Zero xs            = (Nil, xs)
  def (Succ n) (a :- as) = let (xs,ys) = def n as
                           in (a :> xs, ys)

zipStreamWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipStreamWith f (a :- as) (b :- bs) = f a b :- zipStreamWith f as bs


splitStreamI :: NatI n => Stream a -> (List n a, Stream a)
splitStreamI = splitStream nat

nats :: Stream Int
nats = 0 :- fmap (1+) nats

repl :: a -> Stream a
repl a = unfold (\a -> (a,a)) a

cyc :: (1 <= n) => List n a -> Stream a
cyc xs = go (SomeList xs)
  where
  go (SomeList Nil)        = go (SomeList xs)
  go (SomeList (a :> as))  = a :- go (SomeList as)

infixr :>
data List :: Nat -> * -> * where
  Nil  :: List 0 a
  (:>) :: a -> List n a -> List (n + 1) a

data SomeList :: * -> * where
  SomeList :: List n a -> SomeList a

instance Listable (List n) where
  toList Nil        = []
  toList (x :> xs)  = x : toList xs


instance Show a => Show (List n a) where
  show = show . toList

join2 :: List m a -> List n a -> List (m + n) a
join2 Nil xs        = xs
join2 (x :> xs) ys  = x :> join2 xs ys

joinMany :: List m (List n a) -> List (m * n) a
joinMany Nil          = Nil
joinMany (xs :> yss)  = join2 xs (joinMany yss)

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

zipListWith :: (a -> b -> c) -> List n a -> List n b -> List n c
zipListWith f Nil Nil             = Nil
zipListWith f (x :> xs) (y :> ys) = f x y :> zipListWith f xs ys

example :: List 10 (List 3 Int)
example = splitManyI $ fst $ splitStreamI nats

zipListStream :: NatI n => (a -> b -> c) -> Stream a -> List n b -> List n c
zipListStream f l xs = zipListWith f (fst $ splitStreamI l) xs

listHead :: (1 <= n) => List n a -> a
listHead (a :> _) = a

listTail :: List (n + 1) a -> List n a
listTail (_ :> as) = as

listLast :: (1 <= n) => List n a -> a
listLast (a :> Nil)         = a
listLast (_ :> xs@(_ :> _)) = listLast xs

{- TODO:
m = n1 + 1
n = n2 + 1
m + 1 <= n
===>
m <= n2

i.e.

m + 1 <= n2 + 1


listIx :: ((m + 1) <= n) => NatU m -> List n a -> a
listIx Zero (a :> _)      = a
listIx (Succ n) (_ :> xs) = listIx n xs
-}

{-
1 <= n
m = (n1 + 1)
m + n = n2 + 1
==>
n2 = n1 + n0

(n1 + 1) + n = n2 + 1
==>
n2 = n1 + n

listIx2 :: (1 <= n) => NatU m -> List (m + n) a -> a
listIx2 Zero (a :> _)      = a
listIx2 (Succ n) (_ :> xs) = listIx2 n xs
-}

listIx :: (1 <= n) => Nat m -> List (m + n) a -> a
listIx n xs = listHead $ snd $ split2 n xs





