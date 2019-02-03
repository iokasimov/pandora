module Pandora.Paradigm.Structure.Concrete.Stack (Stack, push, top, pop) where

import Pandora.Core.Functor (type (:.:))
import Pandora.Core.Morphism ((.), ($))
import Pandora.Paradigm.Basis.Cofree (Cofree ((:<)), unwrap)
import Pandora.Paradigm.Basis.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Junction.Transformer (Y (Y, y), type (:>:))
import Pandora.Paradigm.Structure.Property.Hollow (Hollow (hollow))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Alternative (Alternative ((<+>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))

type Stack = (Cofree :>: Maybe)

instance Hollow Maybe where
	hollow result _ (Y Nothing) = result
	hollow _ f (Y (Just struct)) = f struct

push :: a -> Stack a -> Stack a
push x (Y struct) = (Y $ (:<) x . Just <$> struct) <+> point x

top :: Stack a -> Maybe a
top (Y struct) = extract <$> struct

pop :: Stack a -> Stack a
pop (Y struct) = Y $ struct >>= unwrap
