module Pandora.Paradigm.Inventory.Storage (Storage (..), Store, position, access, retrofit) where

import Pandora.Core.Functor (type (:.:))
import Pandora.Core.Morphism ((.), ($), (?))
import Pandora.Paradigm.Basis.Identity (Identity)
import Pandora.Paradigm.Basis.Product (Product ((:*)), type (:*))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Comonad (Comonad)

newtype Storage p t a = Storage { stored :: ((:*) p :.: t :.: (->) p) a }

instance Covariant t => Covariant (Storage p t) where
	f <$> Storage (p :* x) = Storage . (:*) p $ (f .) <$> x

instance Extractable t => Extractable (Storage p t) where
	extract (Storage (p :* x)) = extract x p

instance Extendable t => Extendable (Storage p t) where
	Storage (old :* x) =>> f = Storage . (:*) old . (=>>) x $
		\y -> \new -> f . Storage $ new :* y

instance Applicative t => Applicative (Storage p t) where
	Storage (p :* x) <*> Storage (q :* y) = Storage . (:*) q $
		(\f g x -> f x (g x)) <$> x <*> y

instance Comonad g => Comonad (Storage p g) where

type Store p = Storage p Identity

position :: Storage p t a -> p
position (Storage (p :* _)) = p

access :: Extractable t => p -> Storage p t a -> a
access p = extract ? p . extract . stored

retrofit :: Extractable t => (p -> p) -> Storage p t a -> Storage p t a
retrofit f (Storage (p :* x)) = Storage $ (f p) :* x
