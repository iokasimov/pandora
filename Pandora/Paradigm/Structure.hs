{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure (module Exports) where

import Pandora.Paradigm.Structure.Ability as Exports
import Pandora.Paradigm.Structure.Interface as Exports
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

-- instance Substructure Left Delta where
-- 	type Substructural Left Delta = Identity
-- 	substructure (extract . run -> l :^: r) =
-- 		Store $ Identity l :*: lift . (:^: r) . extract
--
-- instance Substructure Right Delta where
-- 	type Substructural Right Delta = Identity
-- 	substructure (extract . run -> l :^: r) =
-- 		Store $ Identity r :*: lift . (l :^:) . extract
--
-- instance Covariant t => Substructure Left (Delta <:.> t) where
-- 	type Substructural Left (Delta <:.> t) = t
-- 	substructure (run . extract . run -> l :^: r) =
-- 		Store $ r :*: lift . unite . (l :^:)
--
-- instance Covariant t => Substructure Right (Delta <:.> t) where
-- 	type Substructural Right (Delta <:.> t) = t
-- 	substructure (run . extract . run -> l :^: r) =
-- 		Store $ l :*: lift . unite . (:^: r)

instance Covariant t => Substructure Tail (Tap t) where
	type Substructural Tail (Tap t) = t
	substructure (extract . run -> Tap x xs) =
		Store $ xs :*: lift . Tap x

instance Morphable Preorder (Construction Wye) where
	type Morphing Preorder (Construction Wye) = Construction Maybe
	morphing (extract . run -> Construct x End) = Construct x Nothing
	morphing (extract . run -> Construct x (Left lst)) = Construct x . Just $ morph @Preorder lst
	morphing (extract . run -> Construct x (Right rst)) = Construct x . Just $ morph @Preorder rst
	morphing (extract . run -> Construct x (Both lst rst)) = Construct x . Just $ morph @Preorder lst + morph @Preorder rst

instance Morphable Inorder (Construction Wye) where
	type Morphing Inorder (Construction Wye) = Construction Maybe
	morphing (extract . run -> Construct x End) = point x
	morphing (extract . run -> Construct x (Left lst)) = morph @Inorder lst + point x
	morphing (extract . run -> Construct x (Right rst)) = point x + morph @Inorder rst
	morphing (extract . run -> Construct x (Both lst rst)) = morph @Inorder lst + point x + morph @Inorder rst

instance Morphable Postorder (Construction Wye) where
	type Morphing Postorder (Construction Wye) = Construction Maybe
	morphing (extract . run -> Construct x End) = point x
	morphing (extract . run -> Construct x (Left lst)) = morph @Postorder lst + point x
	morphing (extract . run -> Construct x (Right rst)) = morph @Postorder rst + point x
	morphing (extract . run -> Construct x (Both lst rst)) = morph @Postorder lst + morph @Postorder rst + point x

instance Morphable o (Construction Wye) => Morphable o Binary where
	type Morphing o Binary = Maybe <:.> Morphing o (Construction Wye)
	morphing = unite . comap (morph @o) . run . extract . run

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
