module Pandora.Paradigm.Structure.Stack (Stack, push, top, pop, filter, linearize) where

import Pandora.Core.Morphism ((.), ($))
import Pandora.Paradigm.Basis.Twister (Twister ((:<)), unwrap)
import Pandora.Paradigm.Basis.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Basis.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Junction.Transformer (Y (Y), type (:>:))
import Pandora.Paradigm.Inventory.Stateful (fold)
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Traversable (Traversable)
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Object.Setoid (bool)

-- | Linear data structure that serves as a collection of elements
type Stack = (Twister :>: Maybe)

push :: a -> Stack a -> Stack a
push x (Y struct) = (Y $ (:<) x . Just <$> struct) <+> point x

top :: Stack a -> Maybe a
top (Y struct) = extract <$> struct

pop :: Stack a -> Stack a
pop (Y struct) = Y $ struct >>= unwrap

filter :: Predicate a -> Stack a -> Stack a
filter (Predicate p) = Y . fold Nothing
	(\x s -> bool s (Just $ x :< s) $ p x)

-- | Transform any traversable structure into a stack
linearize :: Traversable t => t a -> Stack a
linearize = Y . fold Nothing (\x -> Just . (:<) x)
