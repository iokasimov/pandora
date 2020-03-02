{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Environment (Environment (..), Configured, env) where

import Pandora.Core.Functor (Variant (Co))
import Pandora.Core.Morphism (identity, (.), (!), (%))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, unwrap))
import Pandora.Paradigm.Controlflow.Joint.Transformer (Transformer (Schema, lay, wrap), (:>) (T))
import Pandora.Paradigm.Controlflow.Joint.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Controlflow.Joint.Schemes.TU (TU (TU))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Divariant (($))

newtype Environment e a = Environment (e -> a)

instance Covariant (Environment e) where
	f <$> Environment x = Environment $ f . x

instance Pointable (Environment e) where
	point x = Environment (x !)

instance Applicative (Environment e) where
	f <*> x = Environment $ \e -> unwrap f e $ unwrap x e

instance Distributive (Environment e) where
	g >>- f = Environment $ g >>- (unwrap <$> f)

instance Bindable (Environment e) where
	Environment x >>= f = Environment $ \e -> unwrap % e . f . x $ e

instance Monad (Environment e) where

instance Interpreted (Environment e) where
	type Primary (Environment e) a = (->) e a
	unwrap (Environment x) = x

instance Transformer (Environment e) where
	type Schema (Environment e) u = TU 'Co 'Co ((->) e) u
	lay = T . TU . (!)
	wrap x = T. TU $ point <$> unwrap x

type Configured e = Adaptable (Environment e)

instance Covariant u => Covariant (TU 'Co 'Co ((->) e) u) where
	f <$> TU x = TU $ \r -> f <$> x r

instance (Covariant u, Pointable u) => Pointable (TU 'Co 'Co ((->) e) u) where
	point = TU . point . point

instance Applicative u => Applicative (TU 'Co 'Co ((->) e) u) where
	TU f <*> TU x = TU $ \r -> f r <*> x r

instance Bindable u => Bindable (TU 'Co 'Co ((->) e) u) where
	TU x >>= f = TU $ \e -> x e >>= ($ e) . unwrap . f

env :: (Covariant t, Configured e t) => t e
env = adapt $ Environment identity
