{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Stack (Stack, push, top, pop, filter, linearize) where

import Pandora.Core.Functor (type (~>))
import Pandora.Core.Morphism ((&))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (<**>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Object.Setoid (Setoid ((==)), (?))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construction), deconstruct)
import Pandora.Paradigm.Inventory.State (fold)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics (type (:-.))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (run)
import Pandora.Paradigm.Controlflow.Joint.Schemes.TU (TU (TU))
import Pandora.Paradigm.Structure.Variation.Nonempty (Nonempty)

-- | Linear data structure that serves as a collection of elements
type Stack = TU Covariant Covariant Maybe (Construction Maybe)

type instance Nonempty Stack = Construction Maybe

instance Covariant Stack where
	f <$> TU stack = TU $ f <$$> stack

instance Pointable Stack where
	point = TU . Just . point

instance Alternative Stack where
	TU x <+> TU y = TU $ x <+> y

instance Avoidable Stack where
	empty = TU Nothing

instance Applicative Stack where
	TU f <*> TU x = TU $ f <**> x

instance Traversable Stack where
	TU stack ->> f = TU <$> stack ->>> f

instance Setoid a => Setoid (Stack a) where
	TU ls == TU rs = ls == rs

instance Semigroup (Stack a) where
	TU Nothing + TU ys = TU ys
	TU (Just (Construction x xs)) + TU ys = TU . Just . Construction x . run
		$ TU @Covariant @Covariant xs + TU @Covariant @Covariant ys

instance Monoid (Stack a) where
	zero = TU Nothing

top :: Stack a :-. Maybe a
top stack = Store $ (:*:) (extract <$> run stack) $ \case
    Just x -> stack & pop & push x
    Nothing -> pop stack

push :: a -> Stack a -> Stack a
push x (TU stack) = TU $ (Construction x . Just <$> stack) <+> (point . point) x

pop :: Stack ~> Stack
pop (TU stack) = TU $ stack >>= deconstruct

filter :: Predicate a -> Stack a -> Stack a
filter (Predicate p) = TU . fold empty
	(\now new -> p now ? Just (Construction now new) $ new)

-- | Transform any traversable structure into a stack
linearize :: Traversable t => t ~> Stack
linearize = TU . fold Nothing (\x -> Just . Construction x)
