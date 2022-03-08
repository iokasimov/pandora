{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Primary (module Exports, Simplification, twosome) where

import Pandora.Paradigm.Primary.Linear as Exports
import Pandora.Paradigm.Primary.Transformer as Exports
import Pandora.Paradigm.Primary.Functor as Exports
import Pandora.Paradigm.Primary.Object as Exports

import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Core.Functor (type (:.), type (>))
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category ((<--), (<---))
import Pandora.Pattern.Kernel (Kernel (constant))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((|-), (-|)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Paradigm.Algebraic.Exponential (type (-->), type (<--), (&), (%))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Algebraic.Sum ((:+:) (Option, Adoption))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (<~))
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Schemes (TU (TU), T_U (T_U), UT, TUT, P_Q_T (P_Q_T), type (<:.>), type (<:.:>))
-- import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (resolve))
-- import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Into), premorph)
-- import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substance, substructure))

instance Adjoint (->) (->) (Flip (:*:) s) ((->) s) where
	f -| x = \s -> f . Flip <--- x :*: s
	f |- Flip (x :*: s) = f x s

twosome :: t a -> u a -> (<:.:>) t u (:*:) a
twosome x y = T_U <--- x :*: y

type family Simplification (t :: * -> *) (a :: *) where
	Simplification Exactly a = a
	Simplification (TU _ _ t u) a = t :. u > a
	Simplification (UT _ _ t u) a = u :. t > a
	Simplification (TUT _ _ _ t t' u) a = t :. u :. t' > a
	Simplification (T_U _ _ p t u) a = p (t a) (u a)
	Simplification t a = t a
