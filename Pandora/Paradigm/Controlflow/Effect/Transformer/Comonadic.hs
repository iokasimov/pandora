{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Controlflow.Effect.Transformer.Comonadic (Comonadic (..), (:<) (..)) where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Schematic, Interpreted (Primary, run, unite))

class Interpreted t => Comonadic t where
	{-# MINIMAL bring #-}
	bring :: Extractable u (->) => t :< u ~> t

infixr 3 :<
newtype (:<) t u a = TC { tc :: Schematic Comonad t u a }

instance Covariant (Schematic Comonad t u) => Covariant (t :< u) where
	f <$> TC x = TC $ f <$> x

instance Covariant_ (Schematic Comonad t u) (->) (->) => Covariant_ (t :< u) (->) (->) where
	f -<$>- TC x = TC $ f -<$>- x

instance Pointable (Schematic Comonad t u) (->) => Pointable (t :< u) (->) where
	point = TC . point

instance Extractable (Schematic Comonad t u) (->) => Extractable (t :< u) (->) where
	extract = extract . tc

instance Applicative (Schematic Comonad t u) => Applicative (t :< u) where
	TC f <*> TC x = TC $ f <*> x

instance Alternative (Schematic Comonad t u) => Alternative (t :< u) where
	TC x <+> TC y = TC $ x <+> y

instance Traversable (Schematic Comonad t u) (->) (->) => Traversable (t :< u) (->) (->) where
	f <<- TC x = TC -<$>- f <<- x

instance Distributive (Schematic Comonad t u) (->) (->) => Distributive (t :< u) (->) (->) where
	f -<< x = TC $ tc . f -<< x

instance Bindable (Schematic Comonad t u) (->) => Bindable (t :< u) (->) where
	f =<< TC x = TC $ tc . f =<< x

instance Extendable (Schematic Comonad t u) => Extendable (t :< u) where
	TC x =>> f = TC $ x =>> f . TC

instance (Extractable (t :< u) (->), Extendable (t :< u)) => Comonad (t :< u) (->) where

instance Lowerable (Schematic Comonad t) => Lowerable ((:<) t) where
	lower (TC x) = lower x

instance Hoistable (Schematic Comonad t) => Hoistable ((:<) t) where
	f /|\ TC x = TC $ f /|\ x

instance (Interpreted (Schematic Comonad t u)) => Interpreted (t :< u) where
	type Primary (t :< u) a = Primary (Schematic Comonad t u) a
	run ~(TC x) = run x
	unite = TC . unite
