module Paradigm.Identity (Identity (..)) where

import Core.Morphism ((.), ($))
import Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pattern.Functor.Extractable (Extractable (extract))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Applicative (Applicative ((<*>)))
import Pattern.Functor.Traversable (Traversable ((->>)))
import Pattern.Functor.Distributive (Distributive ((>>-)))
import Pattern.Functor.Bindable (Bindable ((>>=)))
import Pattern.Functor.Extendable (Extendable ((=>>)))
import Pattern.Functor.Adjoint (Adjoint (phi, psi))

newtype Identity a = Identity a

instance Covariant Identity where
	f <$> Identity x = Identity $ f x

instance Applicative Identity where
	Identity f <*> Identity x = Identity $ f x

instance Pointable Identity where
	point = Identity

instance Extractable Identity where
	extract (Identity x) = x

instance Traversable Identity where
	Identity x ->> f = Identity <$> f x

instance Distributive Identity where
	x >>- f = Identity $ extract . f <$> x

instance Bindable Identity where
	Identity x >>= f = f x

instance Extendable Identity where
	x =>> f = Identity . f $ x

instance Adjoint Identity Identity where
	phi f = Identity . f . Identity
	psi f = extract . extract . comap f
