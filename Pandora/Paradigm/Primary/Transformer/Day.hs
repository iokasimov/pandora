{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Primary.Transformer.Day where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Transformation.Hoistable (Hoistable ((/|\)))
import Pandora.Pattern.Operation.Exponential ((.:..))

data Day t u a = forall b c . Day (t b) (u c) (b -> c -> a)

instance Covariant (->) (->) (Day t u) where
	f <-|- Day tb uc g = Day tb uc <---- f .:.. g

instance (Extendable (->) t, Extendable (->) u) => Extendable (->) (Day t u) where
	f <<= day@(Day tb uc _) = Day tb uc <--- constant . constant <-- f day

instance Hoistable (->) (Day t) where
	g /|\ Day tb uc bca = Day tb <-- g uc <-- bca

data Day_ category source target t u r = forall a b .
	Day_ (target (category (source a b) r) (target (t a) (u b)))
