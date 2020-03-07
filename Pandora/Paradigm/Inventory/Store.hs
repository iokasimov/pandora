module Pandora.Paradigm.Inventory.Store (Store (..), position, access, retrofit) where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Core.Morphism ((%))
import Pandora.Paradigm.Basis.Product (Product ((:*:)), type (:*:))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Divariant (($))

newtype Store p a = Store { stored :: (:*:) p :. (->) p := a }

instance Covariant (Store p) where
	g <$> Store (p :*: f) = Store . (:*:) p $ (g .) f

instance Extractable (Store p) where
	extract (Store (p :*: f)) = f p

instance Extendable (Store p) where
	Store (old :*: f) =>> g = Store . (:*:) old
		$ \new -> g . Store $ new :*: f

instance Comonad (Store p) where

position :: Store p a -> p
position (Store (p :*: _)) = p

access :: p -> Store p a -> a
access p = extract % p . stored

retrofit :: (p -> p) -> Store p a -> Store p a
retrofit g (Store (p :*: f)) = Store $ g p :*: f
