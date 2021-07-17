{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Imprint (Imprint (..), Traceable) where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), Covariant_ ((-<$>-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant_ ((->$<-)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Distributive (Distributive ((-<<)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Divariant (Divariant ((>->)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Paradigm.Primary.Functor.Function ()
import Pandora.Paradigm.Primary.Transformer.Flip (Flip (Flip))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Schematic, Interpreted (Primary, run, unite, (||=)))
import Pandora.Paradigm.Controlflow.Effect.Transformer.Comonadic (Comonadic (bring), (:<) (TC))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable)
import Pandora.Paradigm.Schemes.UT (UT (UT), type (<.:>))

newtype Imprint e a = Imprint (e -> a)

instance Covariant (Imprint e) where
	f <$> Imprint x = Imprint $ f . x

instance Covariant_ (Imprint e) (->) (->) where
	f -<$>- Imprint g = Imprint $ f . g

instance Contravariant_ (Flip Imprint a) (->) (->) where
	f ->$<- Flip (Imprint g) = Flip . Imprint $ g . f

instance Distributive (Imprint e) (->) (->) where
	f -<< g = Imprint $ (run -<$>- f) -<< g

instance Monoid e => Extractable (Imprint e) (->) where
	extract (Imprint x) = x zero

instance Divariant Imprint (->) (->) (->) where
	(>->) ab cd bc = ab >-> cd ||= bc

instance Semigroup e => Extendable (Imprint e) where
	Imprint x =>> f = Imprint $ \e -> f $ Imprint $ x . (e +)

instance Interpreted (Imprint e) where
	type Primary (Imprint e) a = (->) e a
	run ~(Imprint x) = x
	unite = Imprint

type instance Schematic Comonad (Imprint e) = (<.:>) ((->) e)

instance Monoid e => Comonadic (Imprint e) where
	bring (TC (UT x)) = Imprint . extract $ x

instance {-# OVERLAPS #-} (Semigroup e, Extendable u) => Extendable ((->) e <.:> u) where
	UT x =>> f = UT $ x =>> (\x' e -> f . UT . (<$>) (. (e +)) $ x')

type Traceable e t = Adaptable t (Imprint e)
