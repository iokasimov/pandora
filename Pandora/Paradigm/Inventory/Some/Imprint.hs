{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Inventory.Some.Imprint (Imprint (..), Traceable) where

import Pandora.Core.Interpreted (Interpreted (Primary, run, unite))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>-|-)))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Paradigm.Algebraic ()
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable)
import Pandora.Paradigm.Schemes (Schematic, UT (UT), type (<.:>))

newtype Imprint e a = Imprint (e -> a)

instance Covariant (->) (->) (Imprint e) where
	f <-|- Imprint g = Imprint <-- f . g

instance Contravariant (->) (->) (Flip Imprint a) where
	f >-|- Flip (Imprint g) = Flip . Imprint <-- g . f

instance Distributive (->) (->) (Imprint e) where
	f -<< g = Imprint <--- (run @(->) <-|- f) -<< g

instance Semigroup e => Extendable (->) (Imprint e) where
	f <<= Imprint x = Imprint <-- \e -> f . Imprint <-- x . (e +)

instance Interpreted (->) (Imprint e) where
	type Primary (Imprint e) a = (->) e a
	run ~(Imprint x) = x
	unite = Imprint

type instance Schematic Comonad (Imprint e) = (<.:>) ((->) e)

instance {-# OVERLAPS #-} (Semigroup e, Extendable (->) u) => Extendable (->) ((->) e <.:> u) where
	f <<= UT x = UT <--- (\x' e -> f . UT . (<-|-) (. (e +)) <-- x') <<= x

type Traceable e t = Adaptable t (->) (Imprint e)
