module Pandora.Paradigm.Structure.Stack (Stack, push, top, pop) where

import Pandora.Core.Composition ((:.:))
import Pandora.Core.Morphism ((.))
import Pandora.Paradigm.Basis.Cofree (Cofree ((:<)), unwrap)
import Pandora.Paradigm.Basis.Maybe (Maybe (Just))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))

type Stack a = (Maybe :.: (Cofree Maybe)) a

push :: a -> Stack a -> Stack a
push x stack = ((:<) x . Just <$> stack) <+> (point . point) x

top :: Stack a -> Maybe a
top stack = extract <$> stack

pop :: Stack a -> Stack a
pop stack = stack >>= unwrap
