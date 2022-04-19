module Pandora.Paradigm.Primary.Auxiliary where

import Pandora.Pattern.Category ((<--))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Paradigm.Algebraic.Exponential ()

data Vertical a = Up a | Down a

instance Covariant (->) (->) Vertical where
	f <-|- Up x = Up <-- f x
	f <-|- Down x = Down <-- f x

data Horizontal a = Left a | Right a

instance Covariant (->) (->) Horizontal where
	f <-|- Left x = Left <-- f x
	f <-|- Right x = Right <-- f x
