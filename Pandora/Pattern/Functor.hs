module Pandora.Pattern.Functor (module Exports, Functor (..), Covariant_, Contravariant_, Bindable_) where

import Pandora.Pattern.Functor.Comonad as Exports
import Pandora.Pattern.Functor.Monad as Exports
import Pandora.Pattern.Functor.Representable as Exports
import Pandora.Pattern.Functor.Adjoint as Exports
import Pandora.Pattern.Functor.Extendable as Exports
import Pandora.Pattern.Functor.Bindable as Exports
import Pandora.Pattern.Functor.Distributive as Exports
import Pandora.Pattern.Functor.Traversable as Exports
import Pandora.Pattern.Functor.Monoidal as Exports
import Pandora.Pattern.Functor.Semimonoidal as Exports
import Pandora.Pattern.Functor.Invariant as Exports
import Pandora.Pattern.Functor.Contravariant as Exports
import Pandora.Pattern.Functor.Covariant as Exports

import Pandora.Pattern.Morphism.Flip (Flip)
import Pandora.Pattern.Morphism.Straight (Straight)
import Pandora.Pattern.Morphism.Kleisli (Kleisli)

-- TODO: think about prerequisites on morphisms
-- Semifunctors from Semigroupoids and Functors from Categories?
class Functor source target t where
	(-|-) :: source a b -> target (t a) (t b)

type Covariant_ source target = Functor (Straight source) (Straight target)
type Contravariant_ source target = Functor (Straight source) (Flip target)
type Bindable_ source t = Functor (Kleisli t source) (Straight source) t
type Traversable_ source target = forall u . Functor (Kleisli u source) (Kleisli u target)
