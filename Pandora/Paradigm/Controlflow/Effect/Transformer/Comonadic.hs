{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Controlflow.Effect.Transformer.Comonadic (Comonadic (..), (:<) (..)) where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<), (--<<)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<), (==<<)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=), (<<==)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\)))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (-->))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:)((:*:)))
import Pandora.Paradigm.Primary.Algebraic.One (One (One))
import Pandora.Paradigm.Primary.Algebraic (Extractable, point)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Schematic, Interpreted (Primary, run, unite, (<~~~)))

class Interpreted m t => Comonadic m t where
	{-# MINIMAL bring #-}
	bring :: Extractable u => m ((t :< u) a) (t a)

infixr 3 :<
newtype (:<) t u a = TC { tc :: Schematic Comonad t u a }

instance Covariant (->) (->) (Schematic Comonad t u) => Covariant (->) (->) (t :< u) where
	f <-|- TC x = TC <--- f <-|- x

instance Semimonoidal (-->) (:*:) (:*:) (Schematic Comonad t u) => Semimonoidal (-->) (:*:) (:*:) (t :< u) where
	mult = Straight <-- \(TC f :*: TC x) -> TC
		<---- mult @(-->) @(:*:) @(:*:)
			<~~~ f :*: x

instance Monoidal (-->) (-->) (:*:) (:*:) (Schematic Comonad t u) => Monoidal (-->) (-->) (:*:) (:*:) (t :< u) where
	unit _ = Straight <-- TC . point . (<-- One) . run

instance Traversable (->) (->) (Schematic Comonad t u) => Traversable (->) (->) (t :< u) where
	f <<- TC x = TC <-|- f <<- x

instance Distributive (->) (->) (Schematic Comonad t u) => Distributive (->) (->) (t :< u) where
	f -<< x = TC <--- tc . f --<< x

instance Bindable (->) (Schematic Comonad t u) => Bindable (->) (t :< u) where
	f =<< TC x = TC <--- tc . f ==<< x

instance Extendable (->) (Schematic Comonad t u) => Extendable (->) (t :< u) where
	f <<= TC x = TC <--- f . TC <<== x

instance (Extractable (t :< u), Extendable (->) (t :< u)) => Comonad (->) (t :< u) where

instance Lowerable (->) (Schematic Comonad t) => Lowerable (->) ((:<) t) where
	lower (TC x) = lower x

instance Hoistable (->) (Schematic Comonad t) => Hoistable (->) ((:<) t) where
	f /|\ TC x = TC (f /|\ x)

instance (Interpreted (->) (Schematic Comonad t u)) => Interpreted (->) (t :< u) where
	type Primary (t :< u) a = Primary (Schematic Comonad t u) a
	run ~(TC x) = run x
	unite = TC . unite
