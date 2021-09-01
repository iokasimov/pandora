{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Transformer (module Exports) where

import Pandora.Core.Appliable (Appliable ((!)))
import Pandora.Paradigm.Primary.Transformer.Yoneda as Exports
import Pandora.Paradigm.Primary.Transformer.Tap as Exports
import Pandora.Paradigm.Primary.Transformer.Continuation as Exports
import Pandora.Paradigm.Primary.Transformer.Kan as Exports
import Pandora.Paradigm.Primary.Transformer.Day as Exports
import Pandora.Paradigm.Primary.Transformer.Jet as Exports
import Pandora.Paradigm.Primary.Transformer.Jack as Exports
import Pandora.Paradigm.Primary.Transformer.Outline as Exports
import Pandora.Paradigm.Primary.Transformer.Instruction as Exports
import Pandora.Paradigm.Primary.Transformer.Construction as Exports
import Pandora.Paradigm.Primary.Transformer.Reverse as Exports
import Pandora.Paradigm.Primary.Transformer.Backwards as Exports
import Pandora.Pattern.Morphism.Straight as Exports
import Pandora.Pattern.Morphism.Flip as Exports

import Pandora.Paradigm.Primary.Algebraic.Exponential ((%))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))

instance Appliable (->) c b (->) c b where
	f ! x = f x

instance Appliable (->) a (b -> c) (->) b (a -> c) where
	(!) f = (%) f

instance Appliable (Straight m) c b m c b where
	(!) (Straight m) = m

instance Appliable (Flip m) b c m c b where
	(!) (Flip m) = m

instance Appliable (->) b c (->) e d => Appliable (->) a (b -> c) (->) (a :*: e) d where
	f ! (x :*: y) = f x ! y
