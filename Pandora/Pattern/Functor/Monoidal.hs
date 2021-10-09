module Pandora.Pattern.Functor.Monoidal where

import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal)
import Pandora.Paradigm.Primary.Functor.Proxy (Proxy)

type family Unit (p :: * -> * -> *) = r | r -> p

class Semimonoidal p source target t => Monoidal p q source target t | p target -> source where
	unit :: Proxy source -> p (q (Unit target) a) (t a)
