module Pandora.Paradigm.Primary.Transformer.Instruction (Instruction (..)) where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))

data Instruction t a = Enter a | Instruct (t :. Instruction t := a)

instance Covariant t => Covariant (Instruction t) where
	f <$> Enter x = Enter $ f x
	f <$> Instruct xs = Instruct $ f <$$> xs

instance Covariant t => Pointable (Instruction t) where
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

instance Covariant t => Bindable (Instruction t) where
	Enter x >>= f = f x
	Instruct xs >>= f = Instruct $ (>>= f) <$> xs

instance Traversable t => Traversable (Instruction t) where
	Enter x ->> f = Enter <$> f x
	Instruct xs ->> f = Instruct <$> xs ->>> f

instance (forall u . Covariant u) => Liftable Instruction where
	lift x = Instruct $ Enter <$> x
