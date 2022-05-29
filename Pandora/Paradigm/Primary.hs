{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Primary (module Exports, Simplification) where

import Pandora.Paradigm.Primary.Auxiliary as Exports
import Pandora.Paradigm.Primary.Linear as Exports
import Pandora.Paradigm.Primary.Transformer as Exports
import Pandora.Paradigm.Primary.Functor as Exports
import Pandora.Paradigm.Primary.Object as Exports

import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Core.Functor (type (:.), type (>>>))
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category ((<---))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((|-), (-|)))
import Pandora.Pattern.Operation.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Algebraic.Sum ((:+:))
import Pandora.Pattern.Operation.One (One)
import Pandora.Pattern.Operation.Zero (Zero)
import Pandora.Paradigm.Schemes (TU, T_U, UT, TUT)

instance Adjoint (->) (->) (Flip (:*:) s) ((->) s) where
	f -| x = \s -> f . Flip <--- x :*: s
	f |- Flip (x :*: s) = f x s

type family Simplification (t :: * -> *) (a :: *) where
	Simplification Exactly a = a
	Simplification (TU _ _ t u) a = t :. u >>> a
	Simplification (UT _ _ t u) a = u :. t >>> a
	Simplification (TUT _ _ _ t t' u) a = t :. u :. t' >>> a
	Simplification (T_U _ _ p t u) a = p (t a) (u a)
	Simplification t a = t a

type family Cardinality (t :: * -> *) where
	Cardinality Exactly = One
	Cardinality Maybe = Zero :+: One
