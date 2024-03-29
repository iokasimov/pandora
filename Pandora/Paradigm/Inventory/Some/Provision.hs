{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Inventory.Some.Provision where

import Pandora.Core.Interpreted (Interpreted (Primary, run, unite, (<~)))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (identity, (<--), (<---), (<----))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Operation.Exponential (type (-->), (%))
import Pandora.Paradigm.Algebraic ((<<-|-))
import Pandora.Pattern.Operation.Product ((:*:))
import Pandora.Pattern.Operation.One (One (One))
import Pandora.Paradigm.Algebraic (point)
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Paradigm.Controlflow.Effect.Transformer.Monadic (Monadic (wrap), (:>) (TM))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Inventory.Ability.Gettable (Gettable (Getting, get))
import Pandora.Paradigm.Schemes (Schematic, TU (TU), type (<:.>))

newtype Provision e a = Provision (e -> a)

instance Covariant (->) (->) (Provision e) where
	f <-|- Provision x = Provision <-- f . x

instance Contravariant (->) (->) (Flip Provision a) where
	f >-|- Flip (Provision g) = Flip . Provision <-- g . f

instance Semimonoidal (-->) (:*:) (:*:) (Provision e) where
	mult = Straight <-- Provision . (mult @(-->) <~) . (run <<-|-) . (run @(->) <-|-)

instance Monoidal (-->) (-->) (:*:) (:*:) (Provision e) where
	unit _ = Straight <-- \f -> Provision <-- \_ -> run f One

instance Distributive (->) (->) (Provision e) where
	f -<< g = Provision <--- (run @(->) <-|- f) -<< g

instance Bindable (->) (Provision e) where
	f =<< Provision x = Provision <-- \e -> (run % e) . f . x <-- e

instance Monad (->) (Provision e) where

instance Interpreted (->) (Provision e) where
	type Primary (Provision e) a = (->) e a
	run ~(Provision x) = x
	unite = Provision

type instance Schematic Monad (Provision e) = (<:.>) ((->) e)

instance Monadic (->) (Provision e) where
	wrap x = TM . TU <---- point <-|- run x

type Provided e t = Adaptable t (->) (Provision e)

provided :: Provided e t => t e
provided = adapt <-- Provision identity

instance Gettable Provision where
	type Getting Provision p ouput = Provision p p
	get = Provision identity
