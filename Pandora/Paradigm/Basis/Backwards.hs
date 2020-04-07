module Pandora.Paradigm.Basis.Backwards (Backwards (..)) where

import Pandora.Core.Morphism ((&))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Divariant (($))

newtype Backwards t a = Backwards (t a)

instance Covariant t => Covariant (Backwards t) where
	f <$> Backwards x = Backwards $ f <$> x

instance Pointable t => Pointable (Backwards t) where
	point = Backwards . point

instance Extractable t => Extractable (Backwards t) where
	extract (Backwards x) = extract x

instance Applicative t => Applicative (Backwards t) where
	Backwards f <*> Backwards x = Backwards ((&) <$> x <*> f)
