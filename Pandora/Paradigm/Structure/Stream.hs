module Pandora.Paradigm.Structure.Stream where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct))

type Stream = Construction Identity

repeat :: a -> Stream a
repeat x = Construct x . Identity $ repeat x
