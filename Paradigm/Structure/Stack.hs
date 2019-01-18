module Paradigm.Structure.Stack (Stack, push, top, pop) where

import Core.Composition ((:.:))
import Core.Morphism ((.))
import Paradigm.Basis.Cofree (Cofree ((:<)), unwrap)
import Paradigm.Basis.Maybe (Maybe (Just))
import Pattern.Functor.Covariant (Covariant ((<$>)))
import Pattern.Functor.Pointable (Pointable (point))
import Pattern.Functor.Extractable (Extractable (extract))
import Pattern.Functor.Alternative (Alternative ((<+>)))
import Pattern.Functor.Bindable (Bindable ((>>=)))

type Stack a = (Maybe :.: (Cofree Maybe)) a

push :: a -> Stack a -> Stack a
push x stack = ((:<) x . Just <$> stack) <+> (point . point) x

top :: Stack a -> Maybe a
top stack = extract <$> stack

pop :: Stack a -> Stack a
pop stack = stack >>= unwrap
