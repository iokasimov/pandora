module Pandora.Paradigm.Structure.Stack (Stack, push, top, pop, linearize) where

import Pandora.Core.Functor (type (:.:))
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
type Stack a = (Maybe :.: Twister Maybe) a

push :: a -> Stack a -> Stack a
push x stack = ((:<) x . Just <$> stack) <+> (point . point) x

top :: Stack a -> Maybe a
top stack = extract <$> stack

pop :: Stack a -> Stack a
pop stack = stack >>= unwrap

-- | Transform any traversable structure into a stack
linearize :: Traversable t => t a -> Stack a
linearize = fold Nothing (\x -> Just . (:<) x)
