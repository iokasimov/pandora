module Pandora.Paradigm.Basis.Free (Free (..)) where

import Pandora.Core.Morphism ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pandora.Pattern.Functor.Exclusive (Exclusive (exclusive))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), traverse))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))

data Free t a = Pure a | Impure (t (Free t a))

instance Covariant t => Covariant (Free t) where
	f <$> Pure x = Pure $ f x
	f <$> Impure xs = Impure $ (comap . comap) f xs

instance Covariant t => Pointable (Free t) where
	point = Pure

instance Alternative t => Alternative (Free t) where
	Pure x <+> _ = Pure x
	_ <+> Pure y = Pure y
	Impure xs <+> Impure ys = Impure $ xs <+> ys

instance Exclusive t => Exclusive (Free t) where
	exclusive = Impure exclusive

instance Covariant t => Applicative (Free t) where
	Pure f <*> Pure y = Pure $ f y
	Pure f <*> Impure y = Impure $ comap f <$> y
	Impure f <*> y = Impure $ (<*> y) <$> f

instance Covariant t => Bindable (Free t) where
	Pure x >>= f = f x
	Impure xs >>= f = Impure $ (>>= f) <$> xs

instance Traversable t => Traversable (Free t) where
	Pure x ->> f = Pure <$> f x
	Impure xs ->> f = Impure <$> (traverse . traverse) f xs