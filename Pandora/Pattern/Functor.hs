module Pandora.Pattern.Functor (module Exports, Endofunctor) where

import Pandora.Pattern.Functor.Bivariant as Exports
import Pandora.Pattern.Functor.Divariant as Exports
import Pandora.Pattern.Functor.Comonad as Exports
import Pandora.Pattern.Functor.Monad as Exports
import Pandora.Pattern.Functor.Representable as Exports
import Pandora.Pattern.Functor.Adjoint as Exports
import Pandora.Pattern.Functor.Extendable as Exports
import Pandora.Pattern.Functor.Bindable as Exports
import Pandora.Pattern.Functor.Distributive as Exports
import Pandora.Pattern.Functor.Traversable as Exports
import Pandora.Pattern.Functor.Determinable as Exports
import Pandora.Pattern.Functor.Extractable as Exports
import Pandora.Pattern.Functor.Pointable as Exports
import Pandora.Pattern.Functor.Avoidable as Exports
import Pandora.Pattern.Functor.Semimonoidal as Exports
import Pandora.Pattern.Functor.Alternative as Exports
import Pandora.Pattern.Functor.Invariant as Exports
import Pandora.Pattern.Functor.Contravariant as Exports
import Pandora.Pattern.Functor.Covariant as Exports

type family Endofunctor constraint functor category where
	Endofunctor Covariant_ t category = Covariant_ t category category
	Endofunctor Contravariant_ t category = Contravariant_ t category category
	Endofunctor Traversable t category = Traversable t category category
	Endofunctor Distributive t category = Distributive t category category
