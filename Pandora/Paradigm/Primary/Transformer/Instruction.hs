{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Primary.Transformer.Instruction where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)), Covariant_ ((-<$>-)), (-<$$>-))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)), (-<<-<<-))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Transformer.Hoistable (Hoistable ((/|\), hoist))
import Pandora.Paradigm.Primary.Algebraic.Exponential ()

data Instruction t a = Enter a | Instruct (t :. Instruction t := a)

instance Covariant t => Covariant (Instruction t) where
	f <$> Enter x = Enter $ f x
	f <$> Instruct xs = Instruct $ f <$$> xs

instance Covariant_ t (->) (->) => Covariant_ (Instruction t) (->) (->) where
	f -<$>- Enter x = Enter $ f x
	f -<$>- Instruct xs = Instruct $ f -<$$>- xs

instance Covariant_ t (->) (->) => Pointable (Instruction t) (->) where
	point = Enter

instance Alternative t => Alternative (Instruction t) where
	Enter x <+> _ = Enter x
	_ <+> Enter y = Enter y
	Instruct xs <+> Instruct ys = Instruct $ xs <+> ys

instance Avoidable t => Avoidable (Instruction t) where
	empty = Instruct empty

instance Covariant t => Applicative (Instruction t) where
	Enter f <*> Enter y = Enter $ f y
	Enter f <*> Instruct y = Instruct $ f <$$> y
	Instruct f <*> y = Instruct $ (<*> y) <$> f

instance Covariant_ t (->) (->) => Bindable (Instruction t) (->) where
	f =<< Enter x = f x
	f =<< Instruct xs = Instruct $ (f =<<) -<$>- xs

instance Monad t => Monad (Instruction t) where

instance Traversable t (->) (->) => Traversable (Instruction t) (->) (->) where
	f <<- Enter x = Enter -<$>- f x
	f <<- Instruct xs = Instruct -<$>- f -<<-<<- xs

instance Liftable Instruction where
	lift x = Instruct $ Enter -<$>- x

instance (forall t . Bindable t (->), forall t . Pointable t (->)) => Lowerable Instruction where
	lower (Enter x) = point x
	lower (Instruct xs) = lower =<< xs

instance (forall v . Covariant v) => Hoistable Instruction where
	_ /|\ Enter x = Enter x
	f /|\ Instruct xs = Instruct $ hoist f <$> f xs
