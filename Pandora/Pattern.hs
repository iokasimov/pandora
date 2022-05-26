-- {-# LANGUAGE UndecidableInstances #-}
module Pandora.Pattern (module Exports) where

import Pandora.Pattern.Betwixt as Exports
import Pandora.Pattern.Object as Exports
import Pandora.Pattern.Transformation as Exports
import Pandora.Pattern.Functor as Exports
import Pandora.Pattern.Morphism as Exports
import Pandora.Pattern.Groupoid as Exports
import Pandora.Pattern.Kernel as Exports
import Pandora.Pattern.Category as Exports
import Pandora.Pattern.Semigroupoid as Exports

-- TODO: Bindable -> Bindable_
instance (Semigroupoid source, Bindable source t) => Semigroupoid (Kleisli t source) where
	Kleisli g . Kleisli f = Kleisli ((=<<) g . f)
