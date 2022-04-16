module Pandora.Paradigm.Primary.Transformer.Kan where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>-|-)))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Core.Interpreted (Interpreted (Primary, run, unite))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left_, Right_))

data family Kan (v :: * -> k) (t :: * -> *) (u :: * -> *) b a

data instance Kan Left_ t u b a = Lan ((t b -> a) -> u b)

instance Contravariant (->) (->) (Kan Left_ t u b) where
	f >-|- Lan x = Lan <-- x . (f .)

instance Interpreted (->) (Kan Left_ t u b) where
	type Primary (Kan Left_ t u b) a = (t b -> a) -> u b
	run ~(Lan x) = x
	unite = Lan

data instance Kan Right_ t u b a = Ran ((a -> t b) -> u b)

instance Covariant (->) (->) (Kan Right_ t u b) where
	f <-|- Ran x = Ran <-- x . (. f)

instance Interpreted (->) (Kan Right_ t u b) where
	type Primary (Kan Right_ t u b) a = (a -> t b) -> u b
	run ~(Ran x) = x
	unite = Ran
