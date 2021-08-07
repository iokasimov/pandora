module Pandora.Pattern.Functor.Monoidal where

import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal)

type family Unit (p :: * -> * -> *) :: r
