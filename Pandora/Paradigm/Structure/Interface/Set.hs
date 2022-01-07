{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Interface.Set where

import Pandora.Core.Functor (type (:=))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Object.Setoid (Setoid ((!=)))
import Pandora.Pattern.Object.Semigroup ((+))
import Pandora.Pattern.Object.Quasiring (one)
import Pandora.Paradigm.Primary.Algebraic ()
import Pandora.Paradigm.Primary.Algebraic.Product (attached)
import Pandora.Paradigm.Primary.Algebraic.Exponential ((%))
import Pandora.Paradigm.Primary.Functor.Convergence (Convergence (Convergence))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate, equate)
import Pandora.Paradigm.Primary.Object.Boolean (Boolean)
import Pandora.Paradigm.Primary.Object.Numerator (Numerator (Zero))
import Pandora.Paradigm.Schemes.T_U (type (<:.:>))
import Pandora.Paradigm.Inventory.Ability.Modifiable (modify)
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing), Morph (Find), find)
import Pandora.Paradigm.Inventory.Some.State (State)
import Pandora.Paradigm.Controlflow.Effect (run, (!))

type Set t f a = (Traversable (->) (->) t, Setoid a, Setoid (t a), Morphable (Find f) t)

subset :: forall t f a . (Set t f a, Morphing (Find f) t ~ (Predicate <:.:> Maybe := (->))) => Convergence Boolean := t a
subset = Convergence ! \s ss -> Nothing != (find @f @t @Maybe % s) . equate <<- ss

cardinality :: Traversable (->) (->) t => t a -> Numerator
cardinality s = attached . run @(->) @(State _) % Zero ! constant (modify @State (+ one)) <<- s
