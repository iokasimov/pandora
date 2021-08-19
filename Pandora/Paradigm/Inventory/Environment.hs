{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Environment (Environment (..), Configured, env) where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (identity, ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((-<$>-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((->$<-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Functor.Divariant (Divariant ((>->)))
import Pandora.Paradigm.Primary.Algebraic.Exponential ((%))
import Pandora.Paradigm.Primary.Algebraic ()
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic.One (One (One))
import Pandora.Paradigm.Primary.Algebraic (point)
import Pandora.Paradigm.Primary.Transformer.Flip (Flip (Flip))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Schematic, Interpreted (Primary, run, unite))
import Pandora.Paradigm.Controlflow.Effect.Transformer.Monadic (Monadic (wrap), (:>) (TM))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))

newtype Environment e a = Environment (e -> a)

instance Covariant (Environment e) (->) (->) where
	f -<$>- Environment x = Environment $ f . x

instance Contravariant (Flip Environment a) (->) (->) where
	f ->$<- Flip (Environment g) = Flip . Environment $ g . f

instance Semimonoidal (Environment e) (->) (:*:) (:*:) where
	multiply (x :*: y) = unite $ multiply $ run x :*: run y

instance Monoidal (Environment e) (->) (->) (:*:) (:*:) where
	unit _ f = Environment $ \_ -> f One

instance Distributive (Environment e) (->) (->) where
	f -<< g = Environment $ (run -<$>- f) -<< g

instance Bindable (Environment e) (->) where
	f =<< Environment x = Environment $ \e -> (run % e) . f . x $ e

--instance Monad (Environment e) where

instance Divariant Environment (->) (->) (->) where
	(>->) ab cd bc = Environment $ ab >-> cd $ run bc

instance Interpreted (Environment e) where
	type Primary (Environment e) a = (->) e a
	run ~(Environment x) = x
	unite = Environment

type instance Schematic Monad (Environment e) = (<:.>) ((->) e)

instance Monadic (Environment e) where
	wrap x = TM . TU $ point -<$>- run x

type Configured e = Adaptable (Environment e)

env :: Configured e t => t e
env = adapt $ Environment identity
