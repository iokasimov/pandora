module Pandora.Paradigm.Structure.Stream where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Paradigm.Primary.Functor.Delta (Delta ((:^:)))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct))
import Pandora.Paradigm.Primary.Transformer.Tap (Tap (Tap))
import Pandora.Paradigm.Structure.Ability.Zipper (Zipper)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))

type Stream = Construction Identity

repeat :: a -> Stream a
repeat x = Construct x . Identity $ repeat x

type instance Zipper Stream = Tap (Delta <:.> Stream)
