{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Binary (Binary, insert) where

import Pandora.Core.Morphism ((&), (%), (!))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Object.Chain (Chain ((<=>)), order)
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing), maybe)
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (End, Left, Right, Both))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag), type (:#))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construction))
import Pandora.Paradigm.Controlflow.Joint.Schemes.UT (UT (UT))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (run)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics ((%~))
import Pandora.Paradigm.Structure.Variation.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Variation.Substructure (Substructure (Output, sub))

type Binary = UT Covariant Covariant (Construction Wye) Maybe

instance Covariant Binary where
	f <$> UT g = UT $ f <$$> g

instance Pointable Binary where
	point x = UT . Just . Construction x $ End

instance Traversable Binary where
	UT g ->> f = UT <$> g ->>> f

insert :: Chain a => a -> Binary a -> Binary a
insert x (UT Nothing) = point x
insert x tree@(UT (Just (Construction y _))) = x <=> y & order
	(sub @Left %~ (insert x <$>) $ tree) tree
	(sub @Right %~ (insert x <$>) $ tree)

instance Substructure Left Binary where
	type Output Left Binary a = Left :# Binary a
	sub (UT Nothing) = Store $ (:*:) (Tag $ UT Nothing) $ (UT Nothing !)
	sub  t@(UT (Just (Construction x End))) = Store $ (:*:) (Tag $ UT Nothing) $
		maybe t (UT . Just . Construction x . Left) . run . extract
	sub (UT (Just (Construction x (Left lst)))) = Store $ (:*:) (Tag . UT . Just $ lst) $
		maybe (point x) (UT . Just . Construction x . Left) . run . extract
	sub t@(UT (Just (Construction x (Right rst)))) = Store $ (:*:) (Tag $ UT Nothing) $
		maybe t (UT . Just . Construction x . Both % rst) . run . extract
	sub  (UT (Just (Construction x (Both lst rst)))) = Store $ (:*:) (Tag . UT . Just $ lst) $
		maybe (UT (Just (Construction x (Right rst)))) (UT . Just . Construction x . Both % rst) . run . extract

instance Substructure Right Binary where
	type Output Right Binary a = Right :# Binary a
	sub (UT Nothing) = Store $ Tag (UT Nothing) :*: (!) (UT Nothing)
	sub t@(UT (Just (Construction x End))) = Store $ (:*:) (Tag $ UT Nothing) $
		maybe t (UT . Just . Construction x . Right) . run . extract
	sub t@(UT (Just (Construction x (Left lst)))) = Store $ (:*:) (Tag $ UT Nothing) $
		maybe t (UT . Just . Construction x . Both lst) . run . extract
	sub (UT (Just (Construction x (Right rst)))) = Store $ (:*:) (Tag . UT . Just $ rst) $
		maybe (point x) (UT . Just . Construction x . Right) . run . extract
	sub (UT (Just (Construction x (Both lst rst)))) = Store $ (:*:) (Tag . UT . Just $ rst) $
		maybe (UT (Just (Construction x (Left lst)))) (UT . Just . Construction x . Both lst) . run . extract

type instance Nonempty Binary = Construction Wye

instance Substructure Left (Construction Wye) where
	type Output Left (Construction Wye) a = Maybe (Left :# Construction Wye a)
	sub (Construction x End) = Store $ (:*:) Nothing $ (Construction x End !)
	sub (Construction x (Left lst)) = Store $ (:*:) (Just . Tag $ lst) $
		maybe (Construction x End) (Construction x . Left . extract)
	sub tree@(Construction x (Right rst)) = Store $ (:*:) Nothing $
		maybe tree (Construction x . Both % rst . extract)
	sub (Construction x (Both lst rst)) = Store $ (:*:) (Just . Tag $ lst) $
		maybe (Construction x $ Right rst) (Construction x . Both % rst . extract)

instance Substructure Right (Construction Wye) where
	type Output Right (Construction Wye) a = Maybe (Right :# Construction Wye a)
	sub (Construction x End) = Store $ (:*:) Nothing $ (Construction x End !)
	sub tree@(Construction x (Left lst)) = Store $ (:*:) Nothing $
		maybe tree (Construction x . Both lst . extract)
	sub (Construction x (Right rst)) = Store $ (:*:) (Just . Tag $ rst) $
		maybe (Construction x End) (Construction x . Right . extract)
	sub (Construction x (Both lst rst)) = Store $ (:*:) (Just . Tag $ rst) $
		maybe (Construction x $ Left lst) (Construction x . Both lst . extract)
