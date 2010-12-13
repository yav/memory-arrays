{-# LANGUAGE TypeNaturals, TypeOperators, FlexibleContexts, TypeFamilies #-}
module X where

import GHC.TypeNats


-- These work:
addCommutes      :: Nat (x + y) -> Nat (y + x)
addCommutes       = id

addUnit          :: Nat (0 + x) -> Nat x
addUnit           = id

mulCommutes      ::  Nat (x * y) -> Nat (y * x)
mulCommutes       = id

mulUnit          :: Nat (1 * x) -> Nat x
mulUnit           = id

mulZeroAnihL     :: Nat (0 * x) -> Nat 0
mulZeroAnihL      = id

subSame          :: ((a + b) ~ a) => Nat a -> Nat b
subSame x         = nat :: Nat 0

zeroLeast        :: (a <= 0) => Nat a -> Nat 0
zeroLeast         = id

times2           :: Nat (2 * x) -> Nat (x + x)
times2            = id

addBackFD1       :: ((a + b1) ~ c, (a + b2) ~ c) => Nat b1 -> Nat b2
addBackFD1        = id

addBackFD2       :: ((a1 + b) ~ c, (a2 + b) ~ c) => Nat a1 -> Nat a2
addBackFD2        = id

mulBackFD1       :: ((5 * b1) ~ c, (5 * b2) ~ c) => Nat b1 -> Nat b2
mulBackFD1        = id

mulBackFD2       :: ((a1 * 5) ~ c, (a2 * 5) ~ c) => Nat a1 -> Nat a2
mulBackFD2        = id

-- Reports an error
-- times2Odd     :: Nat (x + x) -> Nat 5
-- times2Odd      = id

plusAssoc1       :: Nat ((x + y) + z) -> Nat (x + (y + z))
plusAssoc1        = id

plusAssoc2       :: Nat (x + (y + z)) -> Nat ((x + y) + z)
plusAssoc2        = id

plusAC           :: Nat (x + (y + z) + d) -> Nat (d + (z + x) + y)
plusAC            = id

timesAssoc       :: Nat ((x * y) * z) -> Nat (x * (y * z))
timesAssoc        = id

timesAssoc2      :: Nat (x * (y * z)) -> Nat ((x * y) * z)
timesAssoc2       = id

timesAC          :: Nat (x * (y * z) * d) -> Nat (d * (z * x) * y)
timesAC           = id

{-
test             :: ((x + c) ~ y, (a + (b + c)) ~ y)
                 => Nat a -> Nat b -> Nat x -> ()
test a b x        = f a b x

f :: ((a + b) ~ x) => Nat a -> Nat b -> Nat x -> ()
f _ _ _ = ()
-}

-- test1             :: (x ~ (a + b)) => Nat a -> Nat b -> Nat x -> ()
test1 a b x        = f1 a b x

f1 :: ((a + b) ~ x, (x + c) ~ y) => Nat a -> Nat b -> Nat x -> ()
f1 _ _ _ = ()

-- These do not work:
{-
zeroMinus        :: ((a + b) ~ 0) => Nat a -> Nat 0
zeroMinus         = id

times3           :: Nat (3 * x) -> Nat (x + x + x)
times3            = id


distrib          :: Nat (x * (y + z)) -> Nat (x * y + x * z)
distrib           = id

genMulBackFD2    :: ((a1 * b) ~ c, (a2 * b) ~ c, 1 <= b) => Nat a1 -> Nat a2
genMulBackFD2     = id

--}

-- [Arithmetic and LEQ]
-- Things like: a :< (a :+ b)

-- [Arithmetic and TypeNat]
-- We may want to add rules for interactions between 'TypeNat'
-- and arithmetic.  This would allow us to simplify 'TypeNat (a :+ b)'
-- to '(TypeNat a, TypeNat b)'.  I am not sure if this is a good idea.
-- The main thing that would be required is more evidnece constructors
-- for doing addition, multiplication, etc.


