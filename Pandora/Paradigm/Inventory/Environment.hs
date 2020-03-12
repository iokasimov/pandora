{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Environment (Environment (..), Configured, env) where

import Pandora.Core.Morphism ((!), (%))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))
import Pandora.Paradigm.Controlflow.Joint.Transformer.Monadic (Monadic (lay, wrap), (:>) (TM))
import Pandora.Paradigm.Controlflow.Joint.Schematic (Schematic)
import Pandora.Paradigm.Controlflow.Joint.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Controlflow.Joint.Schemes.TU (TU (TU))
import Pandora.Pattern.Category (identity, (.))
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
	f <*> x = Environment $ \e -> run f e $ run x e

instance Distributive (Environment e) where
	g >>- f = Environment $ g >>- (run <$> f)

instance Bindable (Environment e) where
	Environment x >>= f = Environment $ \e -> run % e . f . x $ e

instance Monad (Environment e) where

type instance Schematic Monad (Environment e) u = TU Covariant Covariant ((->) e) u

instance Interpreted (Environment e) where
	type Primary (Environment e) a = (->) e a
	run (Environment x) = x

instance Monadic (Environment e) where
	lay = TM . TU . (!)
	wrap x = TM . TU $ point <$> run x

type Configured e = Adaptable (Environment e)

instance Covariant u => Covariant (TU Covariant Covariant ((->) e) u) where
	f <$> TU x = TU $ \r -> f <$> x r

instance (Covariant u, Pointable u) => Pointable (TU Covariant Covariant ((->) e) u) where
	point = TU . point . point

instance Applicative u => Applicative (TU Covariant Covariant ((->) e) u) where
	TU f <*> TU x = TU $ \r -> f r <*> x r

instance Bindable u => Bindable (TU Covariant Covariant ((->) e) u) where
	TU x >>= f = TU $ \e -> x e >>= ($ e) . run . f

env :: Configured e t => t e
env = adapt $ Environment identity
