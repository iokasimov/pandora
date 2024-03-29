{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Controlflow.Effect.Transformer.Monadic (Monadic (..), (:>) (..)) where

import Pandora.Core.Functor (type (<))
import Pandora.Core.Interpreted (Interpreted (Primary, run, unite, (<~~~)))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<-/-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Transformation.Liftable (Liftable (lift))
import Pandora.Pattern.Transformation.Hoistable (Hoistable ((/|\)))
import Pandora.Pattern.Operation.Exponential (type (-->))
import Pandora.Pattern.Operation.Product ((:*:)((:*:)))
import Pandora.Pattern.Operation.Sum ((:+:))
import Pandora.Pattern.Operation.One (One (One))
import Pandora.Paradigm.Algebraic (Pointable, point, empty)
import Pandora.Paradigm.Schemes (Schematic)

class Interpreted m t => Monadic m t where
	{-# MINIMAL wrap #-}
	wrap :: Pointable u => m < t a < (t :> u) a

infixr 8 :>
newtype (:>) t u a = TM { tm :: Schematic Monad t u a }

instance Covariant (->) (->) (Schematic Monad t u) => Covariant (->) (->) (t :> u) where
	f <-|- TM x = TM <---- f <-|- x

instance Semimonoidal (-->) (:*:) (:*:) (Schematic Monad t u) => Semimonoidal (-->) (:*:) (:*:) (t :> u) where
	mult = Straight <-- \(TM f :*: TM x) -> TM
		<---- mult @(-->) @(:*:) @(:*:)
			<~~~ f :*: x

instance Monoidal (-->) (-->) (:*:) (:*:) (Schematic Monad t u) => Monoidal (-->) (-->) (:*:) (:*:) (t :> u) where
	unit _ = Straight <-- TM . point . (<-- One) . run

instance Semimonoidal (-->) (:*:) (:+:) (Schematic Monad t u) => Semimonoidal (-->) (:*:) (:+:) (t :> u) where
	mult = Straight <-- \(TM f :*: TM x) -> TM
		<---- mult @(-->) @(:*:) @(:+:)
			<~~~ f :*: x

instance Monoidal (-->) (-->) (:*:) (:+:) (Schematic Monad t u) => Monoidal (-->) (-->) (:*:) (:+:) (t :> u) where
	unit _ = Straight <-- \_ -> TM empty

instance Traversable (->) (->) (Schematic Monad t u) => Traversable (->) (->) (t :> u) where
	f <-/- TM x = TM <-|-- f <-/- x

instance Distributive (->) (->) (Schematic Monad t u) => Distributive (->) (->) (t :> u) where
	f -<< x = TM <--- tm . f -<< x

instance Bindable (->) (Schematic Monad t u) => Bindable (->) (t :> u) where
	f =<< TM x = TM <--- tm . f =<< x

instance Extendable (->) (Schematic Monad t u) => Extendable (->) (t :> u) where
	f <<= TM x = TM <--- f . TM <<= x

instance (Covariant (->) (->) (Schematic Monad t u), Monoidal (-->) (-->) (:*:) (:*:) (Schematic Monad t u), Bindable (->) (t :> u)) => Monad (->) (t :> u) where

instance Liftable (->) (Schematic Monad t) => Liftable (->) ((:>) t) where
	lift = TM . lift

instance Hoistable (->) (Schematic Monad t) => Hoistable (->) ((:>) t) where
	f /|\ TM x = TM (f /|\ x)

instance (Interpreted (->) (Schematic Monad t u)) => Interpreted (->) (t :> u) where
	type Primary (t :> u) a = Primary (Schematic Monad t u) a
	run ~(TM x) = run x
	unite = TM . unite
