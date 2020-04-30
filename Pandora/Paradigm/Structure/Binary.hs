{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Binary (Binary, insert) where

import Pandora.Core.Morphism ((&), (%), (!))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Object.Chain (Chain ((<=>)), order)
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing), maybe)
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (End, Left, Right, Both))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag), type (:#))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construction))
import Pandora.Paradigm.Controlflow.Joint.Schemes.TU (TU (TU))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (run)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics ((%~))
import Pandora.Paradigm.Structure.Variation.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Variation.Substructure (Substructure (Output, sub))

type Binary = TU Covariant Covariant Maybe (Construction Wye)

insert :: Chain a => a -> Binary a -> Binary a
insert x (TU Nothing) = TU . Just . Construction x $ End
insert x tree@(TU (Just (Construction y _))) = x <=> y & order
	(sub @Left %~ (insert x <$>) $ tree) tree
	(sub @Right %~ (insert x <$>) $ tree)

instance Substructure Left Binary where
	type Output Left Binary a = Binary a
	sub (TU Nothing) = Store $ (:*:) (Tag $ TU Nothing) $ (TU Nothing !)
	sub t@(TU (Just (Construction x End))) = Store $ (:*:) (Tag $ TU Nothing) $
		maybe t (TU . Just . Construction x . Left) . run . extract
	sub (TU (Just (Construction x (Left lst)))) = Store $ (:*:) (Tag . TU . Just $ lst) $
		maybe (TU . Just . Construction x $ End) (TU . Just . Construction x . Left) . run . extract
	sub t@(TU (Just (Construction x (Right rst)))) = Store $ (:*:) (Tag $ TU Nothing) $
		maybe t (TU . Just . Construction x . Both % rst) . run . extract
	sub (TU (Just (Construction x (Both lst rst)))) = Store $ (:*:) (Tag . TU . Just $ lst) $
		maybe (TU (Just (Construction x (Right rst)))) (TU . Just . Construction x . Both % rst) . run . extract

instance Substructure Right Binary where
	type Output Right Binary a = Binary a
	sub (TU Nothing) = Store $ Tag (TU Nothing) :*: (!) (TU Nothing)
	sub t@(TU (Just (Construction x End))) = Store $ (:*:) (Tag $ TU Nothing) $
		maybe t (TU . Just . Construction x . Right) . run . extract
	sub t@(TU (Just (Construction x (Left lst)))) = Store $ (:*:) (Tag $ TU Nothing) $
		maybe t (TU . Just . Construction x . Both lst) . run . extract
	sub (TU (Just (Construction x (Right rst)))) = Store $ (:*:) (Tag . TU . Just $ rst) $
		maybe (TU . Just . Construction x $ End) (TU . Just . Construction x . Right) . run . extract
	sub (TU (Just (Construction x (Both lst rst)))) = Store $ (:*:) (Tag . TU . Just $ rst) $
		maybe (TU (Just (Construction x (Left lst)))) (TU . Just . Construction x . Both lst) . run . extract

type instance Nonempty Binary = Construction Wye

instance Substructure Left (Construction Wye) where
	type Output Left (Construction Wye) a = Maybe (Construction Wye a)
	sub (Construction x End) = Store $ Tag Nothing :*: (Construction x End !)
	sub (Construction x (Left lst)) = Store $ (:*:) (Tag . Just $ lst) $
		maybe (Construction x End) (Construction x . Left) . extract
	sub tree@(Construction x (Right rst)) = Store $ (:*:) (Tag Nothing) $
		maybe tree (Construction x . Both % rst) . extract
	sub (Construction x (Both lst rst)) = Store $ (:*:) (Tag . Just $ lst) $
		maybe (Construction x $ Right rst) (Construction x . Both % rst) . extract

instance Substructure Right (Construction Wye) where
	type Output Right (Construction Wye) a = Maybe (Construction Wye a)
	-- type Output Right (Construction Wye) a = Maybe (Right :# Construction Wye a)
	sub (Construction x End) = Store $ Tag Nothing :*: (Construction x End !)
	sub tree@(Construction x (Left lst)) = Store $ (:*:) (Tag Nothing) $
		maybe tree (Construction x . Both lst) . extract
	sub (Construction x (Right rst)) = Store $ (:*:) (Tag . Just $ rst) $
		maybe (Construction x End) (Construction x . Right) . extract
	sub (Construction x (Both lst rst)) = Store $ (:*:) (Tag . Just $ rst) $
		maybe (Construction x $ Left lst) (Construction x . Both lst) . extract
