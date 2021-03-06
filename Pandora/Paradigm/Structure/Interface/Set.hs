{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Interface.Set where

import Pandora.Core.Functor (type (:=))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Object.Setoid (Setoid ((!=)))
import Pandora.Pattern.Object.Semigroup ((+))
import Pandora.Pattern.Object.Quasiring (one)
import Pandora.Paradigm.Primary.Functor.Function ((!.), (%))
import Pandora.Paradigm.Primary.Functor.Convergence (Convergence (Convergence))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate, equate)
import Pandora.Paradigm.Primary.Functor.Product (attached)
import Pandora.Paradigm.Primary.Object.Boolean (Boolean)
import Pandora.Paradigm.Primary.Object.Numerator (Numerator (Zero))
import Pandora.Paradigm.Schemes.T_U (type (<:.:>))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing), Morph (Find), find)
import Pandora.Paradigm.Inventory.State (State, modify)
import Pandora.Paradigm.Controlflow.Effect (run)

type Set t f a = (Traversable t, Setoid a, Setoid (t a), Morphable (Find f) t)

subset :: forall t f a . (Set t f a, Morphing (Find f) t ~ (Predicate <:.:> Maybe := (->))) => Convergence Boolean := t a
subset = Convergence $ \s ss -> Nothing != ss ->> (find @f @t @Maybe % s) . equate

cardinality :: Traversable t => t a -> Numerator
cardinality s = attached . run @(State _) % Zero $ s ->> (modify @Numerator (+ one) !.)
