{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Equipment (Equipment (..)) where

import Pandora.Paradigm.Basis.Product (Product ((:*:)), type (:*:))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Divariant (($))

newtype Equipment e a = Equipment (e :*: a)

instance Covariant (Equipment e) where
	f <$> Equipment x = Equipment $ f <$> x

instance Extractable (Equipment e) where
	extract (Equipment (_ :*: x)) = x

instance Extendable (Equipment e) where
	Equipment (e :*: x) =>> f = Equipment . (:*:) e . f . Equipment $ e :*: x
