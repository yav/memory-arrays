{-# LANGUAGE TypeNaturals, TypeOperators, FlexibleContexts, TypeFamilies #-}

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

-- Reports an error
-- times2Odd     :: Nat (x + x) -> Nat 5
-- times2Odd      = id


-- These do not work:
{-
zeroMinus        :: ((a + b) ~ 0) => Nat a -> Nat 0
zeroMinus         = id

times3           :: Nat (3 * x) -> Nat (x + x + x)
times3            = id

plusAssoc        :: Nat ((x + y) + z) -> Nat (x + (y + z))
plusAssoc         = id

timesAssoc       :: Nat ((x * y) * z) -> Nat (x * (y * z))
timesAssoc        = id

distrib          :: Nat (x * (y + z)) -> Nat ((x * y) + (x * z))
distrib           = id
-}

-- [Arithmetic and LEQ]
-- Things like: a :< (a :+ b)

-- [Arithmetic and TypeNat]
-- We may want to add rules for interactions between 'TypeNat'
-- and arithmetic.  This would allow us to simplify 'TypeNat (a :+ b)'
-- to '(TypeNat a, TypeNat b)'.  I am not sure if this is a good idea.
-- The main thing that would be required is more evidnece constructors
-- for doing addition, multiplication, etc.


