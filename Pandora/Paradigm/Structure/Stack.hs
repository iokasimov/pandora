module Pandora.Paradigm.Structure.Stack where -- (Stack, push, top, pop, linearize) where

import Pandora.Core.Functor (type (:.:), type (><))
import Pandora.Core.Morphism ((.), ($))
import Pandora.Core.Transformation (type (~>))
import Pandora.Paradigm.Basis.Twister (Twister ((:<)), unwrap)
import Pandora.Paradigm.Basis.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Inventory.Stateful (fold)
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Avoidable (Avoidable (idle))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (<**>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))

-- | Linear data structure that serves as a collection of elements
newtype Stack a = Stack (Maybe :.: Twister Maybe >< a)

instance Covariant Stack where
	f <$> Stack stack = Stack $ f <$$> stack

instance Pointable Stack where
	point x = Stack . Just $ x :< Nothing

instance Alternative Stack where
	Stack stack <+> Stack stack' = Stack $ stack <+> stack'

instance Avoidable Stack where
	idle = Stack Nothing

instance Applicative Stack where
	Stack f <*> Stack x = Stack $ f <**> x

instance Traversable Stack where
	Stack stack ->> f = Stack <$> stack ->>> f

push :: a -> Stack a -> Stack a
push x (Stack stack) = Stack $ ((:<) x . Just <$> stack) <+> (point . point) x

top :: Stack ~> Maybe
top (Stack stack) = extract <$> stack

pop :: Stack ~> Stack
pop (Stack stack) = Stack $ stack >>= unwrap

-- | Transform any traversable structure into a stack
linearize :: Traversable t => t ~> Stack
linearize = Stack . fold Nothing (\x -> Just . (:<) x)
