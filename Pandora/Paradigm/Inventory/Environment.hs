{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Environment (Environment (..), Configured, env) where

import Pandora.Pattern.Category (identity, (.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (<**>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Core.Morphism ((!), (%))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Schematic, Interpreted (Primary, run))
import Pandora.Paradigm.Controlflow.Effect.Transformer.Monadic (Monadic (lay, wrap), (:>) (TM))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))

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

instance Interpreted (Environment e) where
	type Primary (Environment e) a = (->) e a
	run (Environment x) = x

type instance Schematic Monad (Environment e) = (<:.>) ((->) e)

instance Monadic (Environment e) where
	lay = TM . TU . (!)
	wrap x = TM . TU $ point <$> run x

type Configured e = Adaptable (Environment e)

instance Covariant u => Covariant ((->) e <:.> u) where
	f <$> TU x = TU $ f <$$> x

instance (Covariant u, Pointable u) => Pointable ((->) e <:.> u) where
	point = TU . point . point

instance Applicative u => Applicative ((->) e <:.> u) where
	TU f <*> TU x = TU $ f <**> x

instance Bindable u => Bindable ((->) e <:.> u) where
	TU x >>= f = TU $ \e -> x e >>= ($ e) . run . f

env :: Configured e t => t e
env = adapt $ Environment identity
