{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Specific.Stack (Stack, push, top, pop, filter, linearize) where

import Pandora.Core.Functor (type (~>))
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
import Pandora.Paradigm.Basis.Twister (Twister (Twister), untwist)
import Pandora.Paradigm.Inventory.State (fold)
import Pandora.Paradigm.Controlflow.Joint.Schemes.UT (UT (UT))

-- | Linear data structure that serves as a collection of elements
type Stack = UT Covariant Covariant (Twister Maybe) Maybe

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
	ls + rs = fold ls push rs

instance Monoid (Stack a) where
	zero = UT Nothing

push :: a -> Stack a -> Stack a
push x (UT stack) = UT $ (Twister x . Just <$> stack) <+> (point . point) x

top :: Stack ~> Maybe
top (UT stack) = extract <$> stack

pop :: Stack ~> Stack
pop (UT stack) = UT $ stack >>= untwist

filter :: Predicate a -> Stack a -> Stack a
filter (Predicate p) = UT . fold empty
	(\now new -> p now ? Just (Twister now new) $ new)

-- | Transform any traversable structure into a stack
linearize :: Traversable t => t ~> Stack
linearize = UT . fold Nothing (\x -> Just . Twister x)
