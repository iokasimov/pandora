module Pandora.Paradigm.Primary.Transformer.Outline (Outline (..)) where

import Pandora.Core.Morphism ((%))
import Pandora.Pattern.Category (identity, (.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Hoistable (Hoistable (hoist))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))

data Outline t a where
	Line :: a -> Outline t a
	Outlined :: t a -> Outline t (a -> b) -> Outline t b

instance Covariant (Outline t) where
	f <$> Line a = Line $ f a
	f <$> Outlined x y = Outlined x ((.) f <$> y)

instance Pointable (Outline t) where
	point = Line

instance Extractable t => Extractable (Outline t) where
	extract (Line x) = x
	extract (Outlined x y) = extract y $ extract x

instance Applicative (Outline f) where
	Line f <*> y = f <$> y
	Outlined x y <*> z = Outlined x ((%) <$> y <*> z)

instance Liftable Outline where
	lift t = Outlined t (Line identity)

instance Hoistable Outline where
	hoist f (Line x) = Line x
	hoist f (Outlined x y) = Outlined (f x) (hoist f y)

instance (Pointable t, Applicative t) => Interpreted (Outline t) where
	type Primary (Outline t) a = t a
	run (Line x) = point x
	run (Outlined t f) = run f <*> t
