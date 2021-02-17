{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure (module Exports) where

import Pandora.Paradigm.Structure.Ability as Exports
import Pandora.Paradigm.Structure.Interface as Exports
import Pandora.Paradigm.Structure.Modification as Exports
import Pandora.Paradigm.Structure.Some as Exports

import Pandora.Pattern.Category (($), (.))
import Pandora.Pattern.Functor.Covariant (Covariant (comap))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Pointable (point)
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Object.Semigroup ((+))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, unite)
import Pandora.Paradigm.Inventory.Optics ((|>))
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:), attached)
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Both, Left, Right, End))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct))
import Pandora.Paradigm.Primary.Transformer.Tap (Tap (Tap))
import Pandora.Paradigm.Schemes.TU (type (<:.>))

instance Monotonic s a => Monotonic s (s :*: a) where
	reduce f r x = reduce f (f (attached x) r) $ extract x

instance Nullable Maybe where
	null = Predicate $ \case { Just _ -> True ; _ -> False }

instance Substructure Right (Product s) where
	type Substructural Right (Product s) = Identity
	substructure (extract . run -> s :*: x) =
		Store $ Identity x :*: lift . (s :*:) . extract

instance Covariant t => Substructure Tail (Tap t) where
	type Substructural Tail (Tap t) = t
	substructure (extract . run -> Tap x xs) =
		Store $ xs :*: lift . Tap x

instance Morphable (Into (Preorder (Construction Maybe))) (Construction Wye) where
	type Morphing (Into (Preorder (Construction Maybe))) (Construction Wye) = Construction Maybe
	morphing (extract . run -> Construct x End) = Construct x Nothing
	morphing (extract . run -> Construct x (Left lst)) = Construct x . Just $ into @(Preorder (Nonempty Stack)) lst
	morphing (extract . run -> Construct x (Right rst)) = Construct x . Just $ into @(Preorder (Nonempty Stack)) rst
	morphing (extract . run -> Construct x (Both lst rst)) = Construct x . Just $ into @(Preorder (Nonempty Stack)) lst + into @(Preorder (Nonempty Stack)) rst

instance Morphable (Into (Inorder (Construction Maybe))) (Construction Wye) where
	type Morphing (Into (Inorder (Construction Maybe))) (Construction Wye) = Construction Maybe
	morphing (extract . run -> Construct x End) = point x
	morphing (extract . run -> Construct x (Left lst)) = into @(Inorder (Nonempty Stack)) lst + point x
	morphing (extract . run -> Construct x (Right rst)) = point x + into @(Inorder (Nonempty Stack)) rst
	morphing (extract . run -> Construct x (Both lst rst)) = into @(Inorder (Nonempty Stack)) lst + point x + into @(Inorder (Nonempty Stack)) rst

instance Morphable (Into (Postorder (Construction Maybe))) (Construction Wye) where
	type Morphing (Into (Postorder (Construction Maybe))) (Construction Wye) = Construction Maybe
	morphing (extract . run -> Construct x End) = point x
	morphing (extract . run -> Construct x (Left lst)) = into @(Postorder (Nonempty Stack)) lst + point x
	morphing (extract . run -> Construct x (Right rst)) = into @(Postorder (Nonempty Stack)) rst + point x
	morphing (extract . run -> Construct x (Both lst rst)) = into @(Postorder (Nonempty Stack)) lst + into @(Postorder (Nonempty Stack)) rst + point x

-- instance Morphable (Into (Levelorder (Construction Maybe))) (Construction Wye) where
-- 	type Morphing (Into (Levelorder (Construction Maybe))) (Construction Wye) = Construction Maybe
-- 	morphing (extract . run -> Construct x End) = point x
-- 	morphing (extract . run -> Construct x (Left lst)) = point x + into @(Levelorder (Nonempty Stack)) lst
-- 	morphing (extract . run -> Construct x (Right rst)) = point x + into @(Levelorder (Nonempty Stack)) lst
-- 	morphing (extract . run -> Construct x (Both lst rst)) = point x + extract lst + extract rst +

		-- (deconstruct lst :: )

instance Morphable (Into (o ds)) (Construction Wye) => Morphable (Into (o ds)) Binary where
	type Morphing (Into (o ds)) Binary = Maybe <:.> Morphing (Into (o ds)) (Construction Wye)
	morphing = unite . comap (into @(o ds)) . run . extract . run

instance Focusable Left (Product s) where
	type Focusing Left (Product s) a = s
	focusing (extract -> s :*: x) = Store $ s :*: Tag . (:*: x)

instance Focusable Right (Product s) where
	type Focusing Right (Product s) a = a
	focusing (extract -> s :*: x) = Store $ x :*: Tag . (s :*:)

instance Accessible s (s :*: a) where
	access ~(s :*: x) = Store $ s :*: (:*: x)

instance Accessible a (s :*: a) where
	access ~(s :*: x) = Store $ x :*: (s :*:)

instance {-# OVERLAPS #-} Accessible b a => Accessible b (s :*: a) where
	access = access @a |> access @b
