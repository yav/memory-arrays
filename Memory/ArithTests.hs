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

(.<=) :: (a :<= b) => Nat a -> Nat b -> ()
_ .<= _ = ()

zero :: Nat 0
zero = nat

one :: Nat 1
one = nat


-- These work:

addCommutes      :: TypeNat (a :+ b) => Nat a -> Nat b -> ()
addCommutes x y   = (x .+ y) .== (y .+ x)

addUnitL         :: TypeNat a => Nat a -> ()
addUnitL x        = (x .+ zero) .== x

addUnitR         :: TypeNat a => Nat a -> ()
addUnitR x        = (zero .+ x) .== x

mulCommutes      :: TypeNat (a :* b) => Nat a -> Nat b -> ()
mulCommutes x y   = (x .* y) .== (y .* x)

mulUnitL         :: TypeNat a => Nat a -> ()
mulUnitL x        = (x .* one) .== x

mulUnitR         :: TypeNat a => Nat a -> ()
mulUnitR x        = (one .* x) .== x

mulZeroAnihL     :: Nat a -> ()
mulZeroAnihL x    = (zero .* x) .== zero

mulZeroAnihR     :: Nat a -> ()
mulZeroAnihR x    = (x .* zero) .== zero

subUnitR         :: TypeNat a => Nat a -> ()
subUnitR x        = (x .- zero) .== x

subSame          :: Nat a -> ()
subSame x         = (x .- x) .== zero

divUnitR         :: TypeNat a => Nat a -> ()
divUnitR x        = (x ./ one)  .== x


-- These do not work:

-- This should get a better type when we add the "a + b = 0" rule.
zeroMinus x       = zero .- x

timesPlus x       = (nat :: Nat 2) .* x .== (x .+ x)

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


