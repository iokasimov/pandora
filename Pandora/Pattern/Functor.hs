module Pandora.Pattern.Functor (module Exports) where

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

--type family Endofunctor constraint functor category where
	--Endofunctor Covariant t category = Covariant category category t
	--Endofunctor Contravariant t category = Contravariant t category category
	--Endofunctor Traversable t category = Traversable t category category
	--Endofunctor Distributive t category = Distributive t category category
