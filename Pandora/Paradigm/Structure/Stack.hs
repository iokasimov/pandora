module Pandora.Paradigm.Structure.Stack (Stack, push, top, pop, linearize) where

import Pandora.Core.Morphism ((.), ($))
import Pandora.Paradigm.Basis.Cofree (Cofree ((:<)), unwrap)
import Pandora.Paradigm.Basis.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Junction.Transformer (Y (Y, y), type (:>:))
import Pandora.Paradigm.Inventory.Stateful (fold)
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Traversable (Traversable)
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))

-- | Linear data structure that serves as a collection of elements
type Stack = (Cofree :>: Maybe)

push :: a -> Stack a -> Stack a
push x (Y struct) = (Y $ (:<) x . Just <$> struct) <+> point x

top :: Stack a -> Maybe a
top (Y struct) = extract <$> struct

pop :: Stack a -> Stack a
pop (Y struct) = Y $ struct >>= unwrap

-- | Transform any traversable structure into a stack
linearize :: Traversable t => t a -> Stack a
linearize = Y . fold Nothing (\x -> Just . (:<) x)
