module Pandora.Paradigm.Structure.Stack (Stack, push, top, pop) where

import Pandora.Core.Functor (type (:.:))
import Pandora.Core.Morphism ((.), ($))
import Pandora.Paradigm.Basis.Cofree (Cofree ((:<)), unwrap)
import Pandora.Paradigm.Basis.Maybe (Maybe (Just))
import Pandora.Paradigm.Basis.Junction.Transformer (T (T, t), type (:!:))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))

type Stack a = (Cofree Maybe :!: Maybe) a

push :: a -> Stack a -> Stack a
push x (T struct) = (T $ (:<) x . Just <$> struct) <+> point x

top :: Stack a -> Maybe a
top (T struct) = extract <$> struct

pop :: Stack a -> Stack a
pop (T struct) = T $ struct >>= unwrap
