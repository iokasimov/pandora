{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Imprint (Imprint (..), Traceable) where

import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))
import Pandora.Paradigm.Controlflow.Joint.Transformer.Comonadic (Comonadic (flick, bring), (:<) (TC))
import Pandora.Paradigm.Controlflow.Joint.Schematic (Schematic)
import Pandora.Paradigm.Controlflow.Joint.Adaptable (Adaptable)
import Pandora.Paradigm.Controlflow.Joint.Schemes.UT (UT (UT))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>), (<**>)))
import Pandora.Pattern.Functor.Distributive (Distributive ((>>-)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))

newtype Imprint e a = Imprint (e -> a)

instance Covariant (Imprint e) where
	f <$> Imprint x = Imprint $ f . x

instance Distributive (Imprint e) where
	g >>- f = Imprint $ g >>- (run <$> f)

instance Monoid e => Extractable (Imprint e) where
	extract (Imprint x) = x zero

instance Semigroup e => Extendable (Imprint e) where
	Imprint x =>> f = Imprint $ \e -> f $ Imprint $ x . (e +)

instance Interpreted (Imprint e) where
	type Primary (Imprint e) a = (->) e a
	run (Imprint x) = x

type instance Schematic Comonad (Imprint e) u = UT Covariant Covariant ((->) e) u

instance Monoid e => Comonadic (Imprint e) where
	flick (TC (UT x)) = extract <$> x
	bring (TC (UT x)) = Imprint . extract $ x

instance Covariant u => Covariant (UT Covariant Covariant ((->) e) u) where
	f <$> UT x = UT $ f <$$> x

instance Applicative u => Applicative (UT Covariant Covariant ((->) e) u) where
	UT f <*> UT x = UT $ f <**> x

instance (Monoid e, Extractable u) => Extractable (UT Covariant Covariant ((->) e) u) where
	extract (UT x) = extract . extract $ x

instance (Semigroup e, Extendable u) => Extendable (UT Covariant Covariant ((->) e) u) where
	UT x =>> f = UT $ x =>> (\x' e -> f . UT . (<$>) (. (e +)) $ x')

type Traceable e t = Adaptable t (Imprint e)
