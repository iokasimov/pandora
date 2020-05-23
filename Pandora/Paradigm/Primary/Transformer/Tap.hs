module Pandora.Paradigm.Primary.Transformer.Tap (Tap (..), context) where

import Pandora.Core.Functor (type (~>))
import Pandora.Core.Morphism ((%))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>), extend))
import Pandora.Pattern.Functor.Divariant (($))

data Tap t a = Tap a (t a)

instance Covariant t => Covariant (Tap t) where
	f <$> Tap x xs = Tap (f x) $ f <$> xs

instance Avoidable t => Pointable (Tap t) where
	point = Tap % empty

instance Covariant t => Extractable (Tap t) where
	extract (Tap x _) = x

instance Applicative t => Applicative (Tap t) where
	Tap f fs <*> Tap x xs = Tap (f x) $ fs <*> xs

instance Traversable t => Traversable (Tap t) where
	Tap x xs ->> f = Tap <$> f x <*> xs ->> f

instance Extendable t => Extendable (Tap t) where
	x =>> f = Tap (f x) $ context x =>> f . Tap (extract x)

context :: Tap t ~> t
context (Tap _ xs) = xs
