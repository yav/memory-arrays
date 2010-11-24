{-# LANGUAGE TypeNaturals, TypeOperators, FlexibleContexts #-}

import GHC.TypeNats


(.+) :: TypeNat (a :+ b) => Nat a -> Nat b -> Nat (a :+ b)
_ .+ _ = nat

(.-) :: TypeNat b => Nat (a :+ b) -> Nat a -> Nat b
_ .- _ = nat

(.*) :: TypeNat (a :* b) => Nat a -> Nat b -> Nat (a :* b)
_ .* _ = nat

(./) :: TypeNat b => Nat (a :* b) -> Nat a -> Nat b
_ ./ _ = nat

(.==) :: Nat a -> Nat a -> ()
_ .== _ = ()


zero :: Nat 0
zero = nat



-- These work:

addCommutes      :: Nat (x :+ y) -> Nat (y :+ x)
addCommutes       = id

addUnit          :: Nat (0 :+ x) -> Nat x
addUnit           = id

mulCommutes      ::  Nat (x :* y) -> Nat (y :* x)
mulCommutes       = id

mulUnit          :: Nat (1 :* x) -> Nat x
mulUnit           = id

mulZeroAnihL     :: Nat (0 :* x) -> Nat 0
mulZeroAnihL      = id

subSame          :: Nat a -> ()
subSame x         = (x .- x) .== zero

zeroLeast        :: (a :<= 0) => Nat a -> Nat 0
zeroLeast         = id

times2           :: Nat (2 :* x) -> Nat (x :+ x)
times2            = id

-- Reports an error
-- times2Odd     :: Nat (x :+ x) -> Nat 5
-- times2Odd      = id




-- These do not work:

-- This should get a better type when we add the "a + b = 0" rule.
zeroMinus x       = zero .- x

times3 x          = (nat :: Nat 3) .* x .== (x .+ x .+ x)


plusAssoc x y z   = ((x .+ y) .+ z) .== (x .+ (y .+ z))
timesAssoc x y z  = ((x .* y) .* z) .== (x .* (y .* z))

distrib x y z     = (x .* (y .+ z)) .== ((x .* y) .+ (x .* z))

-- [Arithmetic and LEQ]
-- Things like: a :< (a :+ b)

-- [Arithmetic and TypeNat]
-- We may want to add rules for interactions between 'TypeNat'
-- and arithmetic.  This would allow us to simplify 'TypeNat (a :+ b)'
-- to '(TypeNat a, TypeNat b)'.  I am not sure if this is a good idea.
-- The main thing that would be required is more evidnece constructors
-- for doing addition, multiplication, etc.


