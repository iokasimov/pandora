{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Environment (Environment (..), Configured, env, Traced) where

import Pandora.Core.Morphism ((!), (%))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))
import Pandora.Paradigm.Controlflow.Joint.Transformer.Monadic (Monadic (lay, wrap), (:>) (TM))
import Pandora.Paradigm.Controlflow.Joint.Transformer.Comonadic (Comonadic (flick, bring), (:<) (TC))
import Pandora.Paradigm.Controlflow.Joint.Schematic (Schematic)
import Pandora.Paradigm.Controlflow.Joint.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Controlflow.Joint.Schemes.TU (TU (TU))
import Pandora.Paradigm.Controlflow.Joint.Schemes.UT (UT (UT))
import Pandora.Pattern.Category (identity, (.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (<**>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))

newtype Environment e a = Environment (e -> a)

instance Covariant (Environment e) where
	f <$> Environment x = Environment $ f . x

instance Pointable (Environment e) where
	point x = Environment (x !)

instance Monoid e => Extractable (Environment e) where
	extract (Environment x) = x zero

instance Applicative (Environment e) where
	f <*> x = Environment $ \e -> run f e $ run x e

instance Distributive (Environment e) where
	g >>- f = Environment $ g >>- (run <$> f)

instance Bindable (Environment e) where
	Environment x >>= f = Environment $ \e -> run % e . f . x $ e

instance Monad (Environment e) where

instance Semigroup e => Extendable (Environment e) where
	Environment x =>> f = Environment $ \e -> f $ Environment $ x . (e +)

instance Interpreted (Environment e) where
	type Primary (Environment e) a = (->) e a
	run (Environment x) = x

type instance Schematic Monad (Environment e) u = TU Covariant Covariant ((->) e) u

instance Monadic (Environment e) where
	lay = TM . TU . (!)
	wrap x = TM . TU $ point <$> run x

type Configured e = Adaptable (Environment e)

instance Covariant u => Covariant (TU Covariant Covariant ((->) e) u) where
	f <$> TU x = TU $ f <$$> x

instance (Covariant u, Pointable u) => Pointable (TU Covariant Covariant ((->) e) u) where
	point = TU . point . point

instance Applicative u => Applicative (TU Covariant Covariant ((->) e) u) where
	TU f <*> TU x = TU $ f <**> x

instance Bindable u => Bindable (TU Covariant Covariant ((->) e) u) where
	TU x >>= f = TU $ \e -> x e >>= ($ e) . run . f

env :: Configured e t => t e
env = adapt $ Environment identity

type instance Schematic Comonad (Environment e) u = UT Covariant Covariant ((->) e) u

instance Monoid e => Comonadic (Environment e) where
	flick (TC (UT x)) = extract <$> x
	bring (TC (UT x)) = Environment . extract $ x

instance Covariant u => Covariant (UT Covariant Covariant ((->) e) u) where
	f <$> UT x = UT $ f <$$> x

instance Applicative u => Applicative (UT Covariant Covariant ((->) e) u) where
	UT f <*> UT x = UT $ f <**> x

instance (Semigroup e, Extendable u) => Extendable (UT Covariant Covariant ((->) e) u) where
	UT x =>> f = UT $ x =>> (\x' e -> f . UT . (<$>) (. (e +)) $ x')

type Traced e t = Adaptable t (Environment e)
