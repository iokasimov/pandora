{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Primary.Transformer.Instruction where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((-<$>-)), (-<$$>-))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)), (-<<-<<-))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\), hoist))
import Pandora.Paradigm.Primary.Algebraic.Exponential ()
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:)((:*:)))
import Pandora.Paradigm.Primary.Algebraic.One (One (One))
import Pandora.Paradigm.Primary.Algebraic (point)

data Instruction t a = Enter a | Instruct (t :. Instruction t := a)

instance Covariant (->) (->) t => Covariant (->) (->) (Instruction t) where
	f -<$>- Enter x = Enter $ f x
	f -<$>- Instruct xs = Instruct $ f -<$$>- xs

instance (Covariant (->) (->) t, Semimonoidal (->) (:*:) (:*:) t) => Semimonoidal (->) (:*:) (:*:) (Instruction t) where
	multiply (Enter x :*: Enter y) = Enter $ x :*: y
	multiply (Enter x :*: Instruct y) = (x :*:) -<$>- Instruct y
	multiply (Instruct x :*: Enter y) = (:*: y) -<$>- Instruct x
	multiply (Instruct x :*: Instruct y) = Instruct $ multiply @(->) @(:*:) -<$>- multiply (x :*: y)

instance (Covariant (->) (->) t, Semimonoidal (->) (:*:) (:*:) t) => Monoidal (->) (->) (:*:) (:*:) (Instruction t) where
	unit _ f = Enter $ f One

instance Covariant (->) (->) t => Bindable (->) (Instruction t) where
	f =<< Enter x = f x
	f =<< Instruct xs = Instruct $ (f =<<) -<$>- xs

instance Monad t => Monad (Instruction t) where

instance Traversable (->) (->) t => Traversable (->) (->) (Instruction t) where
	f <<- Enter x = Enter -<$>- f x
	f <<- Instruct xs = Instruct -<$>- f -<<-<<- xs

instance Liftable Instruction where
	lift x = Instruct $ Enter -<$>- x

instance (forall t . Bindable (->) t, forall t . Monoidal (->) (->) (:*:) (:*:) t) => Lowerable Instruction where
	lower (Enter x) = point x
	lower (Instruct xs) = lower =<< xs

instance (forall v . Covariant (->) (->) v) => Hoistable Instruction where
	_ /|\ Enter x = Enter x
	f /|\ Instruct xs = Instruct $ hoist f -<$>- f xs
