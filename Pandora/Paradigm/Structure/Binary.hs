{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Binary (Binary, insert, left_sub_tree, right_sub_tree) where

import Pandora.Core.Morphism ((&), (%), (!))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Object.Chain (Chain ((<=>)), order)
import Pandora.Paradigm.Basis.Maybe (Maybe (Just, Nothing), maybe)
import Pandora.Paradigm.Basis.Product (Product ((:*:)))
import Pandora.Paradigm.Basis.Wye (Wye (End, Left, Right, Both))
import Pandora.Paradigm.Basis.Twister (Twister (Twister))
import Pandora.Paradigm.Controlflow.Joint.Schemes.UT (UT (UT))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (run)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics (type (:-.), (%~))

type Binary = UT Covariant Covariant (Twister Wye) Maybe

instance Covariant Binary where
	f <$> UT g = UT $ f <$$> g

instance Pointable Binary where
	point x = UT . Just . Twister x $ End

instance Traversable Binary where
	UT g ->> f = UT <$> g ->>> f

insert :: Chain a => a -> Binary a -> Binary a
insert x (UT Nothing) = point x
insert x tree@(UT (Just (Twister y _))) = x <=> y & order
	(left_sub_tree %~ insert x $ tree) tree (right_sub_tree %~ insert x $ tree)

left_sub_tree :: Binary a :-. Binary a
left_sub_tree (UT Nothing) = Store $ (:*:) (UT Nothing) $ (UT Nothing !)
left_sub_tree t@(UT (Just (Twister x End))) = Store $ (:*:) (UT Nothing) $
	maybe t (UT . Just . Twister x . Left) . run
left_sub_tree (UT (Just (Twister x (Left lst)))) = Store $ (:*:) (UT . Just $ lst) $
	maybe (point x) (UT . Just . Twister x . Left) . run
left_sub_tree t@(UT (Just (Twister x (Right rst)))) = Store $ (:*:) (UT Nothing) $
	maybe t (UT . Just . Twister x . Both % rst) . run
left_sub_tree (UT (Just (Twister x (Both lst rst)))) = Store $ (:*:) (UT . Just $ lst) $
	maybe (UT (Just (Twister x (Right rst)))) (UT . Just . Twister x . Both % rst) . run

right_sub_tree :: Binary a :-. Binary a
right_sub_tree (UT Nothing) = Store $ UT Nothing :*: (!) (UT Nothing)
right_sub_tree t@(UT (Just (Twister x End))) = Store $ (:*:) (UT Nothing) $
	maybe t (UT . Just . Twister x . Right) . run
right_sub_tree t@(UT (Just (Twister x (Left lst)))) = Store $ (:*:) (UT Nothing) $
	maybe t (UT . Just . Twister x . Both lst) . run
right_sub_tree (UT (Just (Twister x (Right rst)))) = Store $ (:*:) (UT . Just $ rst) $
	maybe (point x) (UT . Just . Twister x . Right) . run
right_sub_tree (UT (Just (Twister x (Both lst rst)))) = Store $ (:*:) (UT . Just $ rst) $
	maybe (UT (Just (Twister x (Left lst)))) (UT . Just . Twister x . Both lst) . run
