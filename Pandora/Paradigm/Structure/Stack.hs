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
import Pandora.Paradigm.Basis.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Basis.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Basis.Product (Product ((:*:)))
import Pandora.Paradigm.Basis.Construction (Construction (Construction), untwist)
import Pandora.Paradigm.Inventory.State (fold)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics (type (:-.))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (run)
import Pandora.Paradigm.Controlflow.Joint.Schemes.UT (UT (UT))
import Pandora.Paradigm.Structure.Variation.Nonempty (Nonempty)

-- | Linear data structure that serves as a collection of elements
type Stack = UT Covariant Covariant (Construction Maybe) Maybe

type instance Nonempty Stack = Construction Maybe

instance Covariant Stack where
	f <$> UT stack = UT $ f <$$> stack

instance Pointable Stack where
	point = UT . Just . point

instance Alternative Stack where
	UT x <+> UT y = UT $ x <+> y

instance Avoidable Stack where
	empty = UT Nothing

instance Applicative Stack where
	UT f <*> UT x = UT $ f <**> x

instance Traversable Stack where
	UT stack ->> f = UT <$> stack ->>> f

instance Setoid a => Setoid (Stack a) where
	UT ls == UT rs = ls == rs

instance Semigroup (Stack a) where
	UT Nothing + UT ys = UT ys
	UT (Just (Construction x xs)) + UT ys = UT . Just . Construction x . run
		$ UT @Covariant @Covariant xs + UT @Covariant @Covariant ys

instance Monoid (Stack a) where
	zero = UT Nothing

top :: Stack a :-. Maybe a
top stack = Store $ (:*:) (extract <$> run stack) $ \case
    Just x -> stack & pop & push x
    Nothing -> pop stack

push :: a -> Stack a -> Stack a
push x (UT stack) = UT $ (Construction x . Just <$> stack) <+> (point . point) x

pop :: Stack ~> Stack
pop (UT stack) = UT $ stack >>= untwist

filter :: Predicate a -> Stack a -> Stack a
filter (Predicate p) = UT . fold empty
	(\now new -> p now ? Just (Construction now new) $ new)

-- | Transform any traversable structure into a stack
linearize :: Traversable t => t ~> Stack
linearize = UT . fold Nothing (\x -> Just . Construction x)
