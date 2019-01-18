module Paradigm.Inventory.Storage (Storage (..), Store) where

import Core.Composition ((:.:))
import Core.Morphism ((.), ($), flip)
import Paradigm.Basis.Identity (Identity)
import Paradigm.Basis.Product (Product ((:&:)), type (:&:))
import Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pattern.Functor.Extractable (Extractable (extract))
import Pattern.Functor.Extendable (Extendable ((=>>)))
import Pattern.Functor.Comonad (Comonad)

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
