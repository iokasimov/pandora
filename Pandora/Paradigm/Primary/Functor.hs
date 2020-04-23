module Pandora.Paradigm.Primary.Functor (module Exports, note, hush, left, right, this, that) where

import Pandora.Paradigm.Primary.Functor.Fix as Exports
import Pandora.Paradigm.Primary.Functor.Predicate as Exports
import Pandora.Paradigm.Primary.Functor.Variation as Exports
import Pandora.Paradigm.Primary.Functor.Validation as Exports
import Pandora.Paradigm.Primary.Functor.Wye as Exports
import Pandora.Paradigm.Primary.Functor.Edges as Exports
import Pandora.Paradigm.Primary.Functor.Conclusion as Exports
import Pandora.Paradigm.Primary.Functor.Maybe as Exports
import Pandora.Paradigm.Primary.Functor.Endo as Exports
import Pandora.Paradigm.Primary.Functor.Proxy as Exports
import Pandora.Paradigm.Primary.Functor.Tagged as Exports
import Pandora.Paradigm.Primary.Functor.Product as Exports
import Pandora.Paradigm.Primary.Functor.Constant as Exports
import Pandora.Paradigm.Primary.Functor.Identity as Exports

import Pandora.Core.Morphism ((!))
import Pandora.Core.Functor (type (~>))

note :: e -> Maybe ~> Conclusion e
note x = maybe (Failure x) Success

hush :: Conclusion e ~> Maybe
hush = conclusion (Nothing !) Just

left :: Wye ~> Maybe
left (Both ls _) = Just ls
left (Left ls) = Just ls
left (Right _) = Nothing
left End = Nothing

right :: Wye ~> Maybe
right (Both _ rs) = Just rs
right (Left _) = Nothing
right (Right rs) = Just rs
right End = Nothing

this :: Variation e ~> Maybe
this (This x) = Just x
this (That _) = Nothing
this (These _ x) = Just x

that :: Variation e a -> Maybe e
that (This _) = Nothing
that (That x) = Just x
that (These y _) = Just y
