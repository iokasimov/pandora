module Pandora.Paradigm.Basis.Product (Product (..), type (:&:), delta, swap) where

import Pandora.Core.Morphism (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Comonad (Comonad)

infixr 1 :&:

data Product a b = a :&: b

type (:&:) = Product

instance Covariant (Product a) where
	f <$> (x :&: y) = x :&: f y

instance Extractable (Product a) where
	extract (x :&: y) = y

instance Extendable (Product a) where
	(x :&: y) =>> f = (:&:) x $ f (x :&: y)

instance Comonad (Product a) where

delta :: a -> a :&: a
delta x = x :&: x

swap :: a :&: b -> b :&: a
swap (x :&: y) = y :&: x