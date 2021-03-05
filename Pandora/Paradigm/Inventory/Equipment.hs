{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Equipment (Equipment (..), retrieve) where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:), attached)
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Controlflow.Effect.Transformer.Comonadic (Comonadic (bring), (:<) (TC))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Schematic, Interpreted (Primary, run, unite))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))

newtype Equipment e a = Equipment (e :*: a)

instance Covariant (Equipment e) where
	f <$> Equipment x = Equipment $ f <$> x

instance Extractable (Equipment e) where
	extract = extract . run

instance Traversable (Equipment e) where
	Equipment x ->> f = Equipment <$> x ->> f

instance Extendable (Equipment e) where
	Equipment (e :*: x) =>> f = Equipment . (:*:) e . f . Equipment $ e :*: x

instance Interpreted (Equipment e) where
	type Primary (Equipment e) a = e :*: a
	run ~(Equipment x) = x
	unite = Equipment

type instance Schematic Comonad (Equipment e) = (<:.>) ((:*:) e)

instance Comonadic (Equipment e) where
	bring (TC (TU x)) = Equipment $ extract <$> x

type Equipped e t = Adaptable t (Equipment e)

instance {-# OVERLAPS #-} Extendable u => Extendable ((:*:) e <:.> u) where
	TU (e :*: x) =>> f = TU . (:*:) e $ x =>> f . TU . (:*:) e

instance Comonad (Equipment e) where

retrieve :: Equipped e t => t a -> e
retrieve = attached . run @(Equipment _) . adapt
