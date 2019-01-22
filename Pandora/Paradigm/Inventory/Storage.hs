module Pandora.Paradigm.Inventory.Storage (Storage (..), Store) where

import Pandora.Core.Functor (type (:.:))
import Pandora.Core.Morphism ((.), ($), flip)
import Pandora.Paradigm.Basis.Identity (Identity)
import Pandora.Paradigm.Basis.Product (Product ((:&:)), type (:&:))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Comonad (Comonad)

newtype Storage p t a = Storage { stored :: ((:&:) p :.: t :.: (->) p) a }

instance Covariant g => Covariant (Storage p g) where
	f <$> Storage (p :&: x) = Storage . (:&:) p $ (f .) <$> x

instance Extractable g => Extractable (Storage p g) where
	extract (Storage (p :&: x)) = extract x p

instance Extendable g => Extendable (Storage p g) where
	Storage (old :&: x) =>> f = Storage . (:&:) old . (=>>) x $
		\y -> \new -> f . Storage $ new :&: y

instance Comonad g => Comonad (Storage p g) where

type Store p = Storage p Identity

position :: Storage p t a -> p
position (Storage (p :&: _)) = p

access :: Extractable t => p -> Storage p t a -> a
access p = flip extract p . extract . stored
