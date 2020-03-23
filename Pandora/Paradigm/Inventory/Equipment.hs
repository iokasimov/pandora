{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Equipment (Equipment (..), retrieve) where

import Pandora.Paradigm.Basis.Product (Product ((:*:)), type (:*:), attached)
import Pandora.Paradigm.Controlflow.Joint.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Controlflow.Joint.Transformer.Comonadic (Comonadic (flick, bring), (:<) (TC))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))
import Pandora.Paradigm.Controlflow.Joint.Schematic (Schematic)
import Pandora.Paradigm.Controlflow.Joint.Schemes.TU (TU (TU))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Divariant (($))

newtype Equipment e a = Equipment (e :*: a)

instance Covariant (Equipment e) where
	f <$> Equipment x = Equipment $ f <$> x

instance Extractable (Equipment e) where
	extract = extract . run

instance Extendable (Equipment e) where
	Equipment (e :*: x) =>> f = Equipment . (:*:) e . f . Equipment $ e :*: x

instance Interpreted (Equipment e) where
	type Primary (Equipment e) a = e :*: a
	run (Equipment x) = x

type instance Schematic Comonad (Equipment e) u = TU Covariant Covariant ((:*:) e) u

instance Comonadic (Equipment e) where
	flick (TC (TU x)) = extract x
	bring (TC (TU x)) = Equipment $ extract <$> x

type Equipped e t = Adaptable t (Equipment e)

instance Covariant u => Covariant (TU Covariant Covariant ((:*:) e) u) where
	f <$> TU x = TU $ f <$$> x

instance Extractable u => Extractable (TU Covariant Covariant ((:*:) e) u) where
	extract (TU x) = extract . extract $ x

instance Extendable u => Extendable (TU Covariant Covariant ((:*:) e) u) where
	TU (e :*: x) =>> f = TU . (:*:) e $ x =>> f . TU . (:*:) e

instance Comonad (Equipment e) where

retrieve :: Equipped e t => t a -> e
retrieve = attached . run @(Equipment _) . adapt
