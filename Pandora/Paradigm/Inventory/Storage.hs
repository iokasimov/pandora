module Pandora.Paradigm.Inventory.Storage (Storage (..), position, access, retrofit) where

import Pandora.Core.Functor (type (:.), type (>))
import Pandora.Core.Morphism ((.), (?))
import Pandora.Paradigm.Basis.Product (Product ((:*:)), type (:*:))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Divariant (($))

newtype Storage p a = Storage { stored :: (:*:) p :. (->) p > a }

instance Covariant (Storage p) where
	g <$> Storage (p :*: f) = Storage . (:*:) p $ (g .) f

instance Extractable (Storage p) where
	extract (Storage (p :*: f)) = f p

instance Extendable (Storage p) where
	Storage (old :*: f) =>> g = Storage . (:*:) old
		$ \new -> g . Storage $ new :*: f

instance Comonad (Storage p) where

position :: Storage p a -> p
position (Storage (p :*: _)) = p

access :: p -> Storage p a -> a
access p = extract ? p . stored

retrofit :: (p -> p) -> Storage p a -> Storage p a
retrofit g (Storage (p :*: f)) = Storage $ g p :*: f
