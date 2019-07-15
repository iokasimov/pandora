module Pandora.Paradigm.Structure.Stack (Stack, push, top, pop, filter, linearize) where

import Pandora.Core.Functor (type (:.:), type (><))
import Pandora.Core.Morphism ((.), ($))
import Pandora.Core.Transformation (type (~>))
import Pandora.Paradigm.Basis.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Basis.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Basis.Twister (Twister ((:<)), untwist)
import Pandora.Paradigm.Inventory.Stateful (fold)
import Pandora.Paradigm.Junction.Composition (Composition (Primary, unwrap))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (<**>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Object.Setoid (bool)

-- | Linear data structure that serves as a collection of elements
newtype Stack a = Stack (Maybe :.: Twister Maybe >< a)

instance Covariant Stack where
	f <$> Stack stack = Stack $ f <$$> stack

instance Pointable Stack where
	point x = Stack . Just $ x :< Nothing

instance Alternative Stack where
	Stack stack <+> Stack stack' = Stack $ stack <+> stack'

instance Avoidable Stack where
	empty = Stack Nothing

instance Applicative Stack where
	Stack f <*> Stack x = Stack $ f <**> x

instance Traversable Stack where
	Stack stack ->> f = Stack <$> stack ->>> f

instance Composition Stack where
	type Primary Stack a = Maybe :.: Twister Maybe >< a
	unwrap (Stack stack) = stack

push :: a -> Stack a -> Stack a
push x (Stack stack) = Stack $ ((:<) x . Just <$> stack) <+> (point . point) x

top :: Stack ~> Maybe
top (Stack stack) = extract <$> stack

pop :: Stack ~> Stack
pop (Stack stack) = Stack $ stack >>= untwist

filter :: Predicate a -> Stack a -> Stack a
filter (Predicate p) = Stack . fold empty
	(\now new -> bool new (Just $ now :< new) $ p now)

-- | Transform any traversable structure into a stack
linearize :: Traversable t => t ~> Stack
linearize = Stack . fold Nothing (\x -> Just . (:<) x)
