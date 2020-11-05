module Pandora.Pattern (module Exports, (.|..), (.|...), (.|....)) where

import Pandora.Pattern.Object as Exports
import Pandora.Pattern.Transformer as Exports
import Pandora.Pattern.Functor as Exports
import Pandora.Pattern.Category as Exports

import Pandora.Core.Functor (type (:.), type (:=))

(.|..) :: (Category v, Covariant (v a))
	=> v c d -> v a :. v b := c -> v a :. v b := d
f .|.. g = (f .) <$> g

(.|...) :: (Category v, Covariant (v a), Covariant (v b))
	=> v d e -> v a :. v b :. v c := d -> v a :. v b :. v c := e
f .|... g = (f .) <$$> g

(.|....) :: (Category v, Covariant (v a), Covariant (v b), Covariant (v c))
	=> v e f -> v a :. v b :. v c :. v d := e -> v a :. v b :. v c :. v d := f
f .|.... g = (f .) <$$$> g
