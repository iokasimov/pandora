{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Transformer (module Exports, Opposite, Appliable ((!))) where

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
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

instance Interpreted (Flip v a) where
	type Primary (Flip v a) e = v e a
	run ~(Flip x) = x
	unite = Flip

instance Interpreted (Straight v e) where
	type Primary (Straight v e) a = v e a
	run ~(Straight x) = x
	unite = Straight

type family Opposite m where
	Opposite Straight = Flip
	Opposite Flip = Straight

class Appliable m a b n c d where
	(!) :: m a b -> n c d

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
