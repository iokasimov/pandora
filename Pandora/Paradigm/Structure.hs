{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure (module Exports) where

import Pandora.Paradigm.Structure.Ability as Exports
import Pandora.Paradigm.Structure.Interface as Exports
import Pandora.Paradigm.Structure.Modification as Exports
import Pandora.Paradigm.Structure.Some as Exports

import Pandora.Pattern.Category (($), (.), (#))
import Pandora.Pattern.Functor.Covariant (Covariant (comap))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Pointable (point)
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Pattern.Object.Semigroup ((+))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (||=))
import Pandora.Paradigm.Inventory.Optics ()
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:), attached)
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Both, Left, Right, End))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct))
import Pandora.Paradigm.Primary.Transformer.Flip (Flip (Flip))
import Pandora.Paradigm.Primary.Transformer.Tap (Tap (Tap))
import Pandora.Paradigm.Schemes.TU (type (<:.>))
import Pandora.Paradigm.Schemes.PQ_ (PQ_ (PQ_))
import Pandora.Paradigm.Schemes.P_T (P_T (P_T))

instance Monotonic s a => Monotonic s (s :*: a) where
	reduce f r x = reduce f # f (attached x) r # extract x

instance Nullable Maybe where
	null = Predicate $ \case { Just _ -> True ; _ -> False }

instance Covariant t => Substructure Tail (Tap t) where
	type Substructural Tail (Tap t) = t
	substructure = PQ_ $ \tap -> P_T $ case extract # run tap of
		Tap x xs -> Store $ Identity xs :*: lift . Tap x . extract

instance Morphable (Into (Preorder (Construction Maybe))) (Construction Wye) where
	type Morphing (Into (Preorder (Construction Maybe))) (Construction Wye) = Construction Maybe
	morphing (premorph -> Construct x End) = Construct x Nothing
	morphing (premorph -> Construct x (Left lst)) = Construct x . Just $ into @(Preorder (Nonempty List)) lst
	morphing (premorph -> Construct x (Right rst)) = Construct x . Just $ into @(Preorder (Nonempty List)) rst
	morphing (premorph -> Construct x (Both lst rst)) = Construct x . Just $ into @(Preorder (Nonempty List)) lst + into @(Preorder (Nonempty List)) rst

instance Morphable (Into (Inorder (Construction Maybe))) (Construction Wye) where
	type Morphing (Into (Inorder (Construction Maybe))) (Construction Wye) = Construction Maybe
	morphing (premorph -> Construct x End) = point x
	morphing (premorph -> Construct x (Left lst)) = into @(Inorder (Nonempty List)) lst + point x
	morphing (premorph -> Construct x (Right rst)) = point x + into @(Inorder (Nonempty List)) rst
	morphing (premorph -> Construct x (Both lst rst)) = into @(Inorder (Nonempty List)) lst + point x + into @(Inorder (Nonempty List)) rst

instance Morphable (Into (Postorder (Construction Maybe))) (Construction Wye) where
	type Morphing (Into (Postorder (Construction Maybe))) (Construction Wye) = Construction Maybe
	morphing (premorph -> Construct x End) = point x
	morphing (premorph -> Construct x (Left lst)) = into @(Postorder (Nonempty List)) lst + point x
	morphing (premorph -> Construct x (Right rst)) = into @(Postorder (Nonempty List)) rst + point x
	morphing (premorph -> Construct x (Both lst rst)) = into @(Postorder (Nonempty List)) lst + into @(Postorder (Nonempty List)) rst + point x

instance Morphable (Into (o ds)) (Construction Wye) => Morphable (Into (o ds)) Binary where
	type Morphing (Into (o ds)) Binary = Maybe <:.> Morphing (Into (o ds)) (Construction Wye)
	morphing (premorph -> xs) = comap (into @(o ds)) ||= xs

instance Substructure Left (Flip Product a) where
	type Substructural Left (Flip Product a) = Identity
	substructure = PQ_ $ \product -> P_T $ case run # lower product of
		s :*: x -> Store $ Identity (Identity s) :*: lift . Flip . (:*: x) . extract . extract

instance Substructure Right (Product s) where
	type Substructural Right (Product s) = Identity
	substructure = PQ_ $ \product -> P_T $ case lower product of
		s :*: x -> Store $ Identity (Identity x) :*: lift . (s :*:) . extract . extract

instance Accessible s (s :*: a) where
	access = PQ_ $ \(s :*: x) -> P_T . Store $ Identity s :*: (:*: x) . extract

instance Accessible a (s :*: a) where
	access = PQ_ $ \(s :*: x) -> P_T . Store $ Identity x :*: (s :*:) . extract

instance {-# OVERLAPS #-} Accessible b a => Accessible b (s :*: a) where
	access = access @b . access @a
