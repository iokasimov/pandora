module Pandora.Pattern.Functor.Monoidal where

import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal)
import Pandora.Paradigm.Primary.Functor.Proxy (Proxy)

type family Unit (p :: * -> * -> *) = r | r -> p

class Semimonoidal t p source target => Monoidal t p q source target where
	unit :: Proxy source -> p (q (Unit target) a) (t a)
