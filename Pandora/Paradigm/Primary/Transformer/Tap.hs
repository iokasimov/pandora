module Pandora.Paradigm.Primary.Transformer.Tap where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable (hoist))
import Pandora.Paradigm.Primary.Functor.Function ((%), (/))

data Tap t a = Tap a (t a)

instance Covariant t => Covariant (Tap t) where
	f <$> Tap x xs = Tap / f x / f <$> xs

instance Avoidable t => Pointable (Tap t) where
	point = Tap % empty

instance Covariant t => Extractable (Tap t) where
	extract (Tap x _) = x

instance Applicative t => Applicative (Tap t) where
	Tap f fs <*> Tap x xs = Tap / f x / fs <*> xs

instance Traversable t => Traversable (Tap t) where
	Tap x xs ->> f = Tap <$> f x <*> (xs ->> f)

instance (Extractable t, Alternative t, Bindable t) => Bindable (Tap t) where
	Tap x xs >>= f = case f x of ~(Tap y ys) -> Tap y $ ys <+> (xs >>= lower . f)

instance Extendable t => Extendable (Tap t) where
	x =>> f = Tap / f x $ lower x =>> f . Tap (extract x)

instance Lowerable Tap where
	lower (Tap _ xs) = xs

instance Hoistable Tap where
	hoist f (Tap x xs) = Tap x / f xs
