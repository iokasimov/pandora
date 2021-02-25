{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Some.Binary where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), comap))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Avoidable (empty)
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Primary.Object.Ordering (order)
import Pandora.Paradigm.Primary.Object.Numerator (Numerator (Numerator, Zero))
import Pandora.Paradigm.Primary.Object.Denumerator (Denumerator (One))
import Pandora.Paradigm.Primary.Functor.Function ((!), (%), (&))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:), attached, twosome)
import Pandora.Paradigm.Primary.Functor.Wye (Wye (End, Left, Right, Both))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Schemes (TU (TU), T_U (T_U), type (<:.>))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Inventory.State (State, modify)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics (over)
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Nullable (Nullable (null))
import Pandora.Paradigm.Structure.Ability.Focusable (Focusable (Focusing, focusing), Location (Root))
import Pandora.Paradigm.Structure.Ability.Measurable (Measurable (Measural, measurement), Scale (Heighth), measure)
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (resolve))
import Pandora.Paradigm.Structure.Ability.Insertable (Insertable ((+=)))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Rotate, Into), premorph)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substructural, substructure), sub, substitute)
import Pandora.Paradigm.Structure.Ability.Zipper (Zipper)

type Binary = Maybe <:.> Construction Wye

rebalance :: Chain a => (Wye :. Construction Wye := a) -> Nonempty Binary a
rebalance (Both x y) = extract x <=> extract y & order
	(Construct (extract y) $ Both x (rebalance $ deconstruct y))
	(Construct (extract x) $ Both (rebalance $ deconstruct x) (rebalance $ deconstruct y))
	(Construct (extract x) $ Both (rebalance $ deconstruct x) y)

instance (forall a . Chain a) => Insertable Binary where
	x += (run -> Nothing) = lift . Construct x $ End
	x += tree@(run -> Just nonempty) = x <=> extract nonempty & order
		(tree & substitute @Left (x +=)) tree (tree & substitute @Right (x +=))

instance (forall a . Chain a) => Focusable Root Binary where
	type Focusing Root Binary a = Maybe a
	focusing (run . extract -> Nothing) = Store $ Nothing :*: Tag . TU . comap (Construct % End)
	focusing (run . extract -> Just x) = Store $ Just (extract x) :*: Tag . lift . resolve (Construct % deconstruct x) (rebalance $ deconstruct x)

instance Measurable Heighth Binary where
	type Measural Heighth Binary a = Numerator
	measurement (run . extract -> Just bt) = Numerator $ measure @Heighth bt
	measurement (run . extract -> Nothing) = Zero

instance Nullable Binary where
	null = Predicate $ \case { TU Nothing -> True ; _ -> False }

instance Substructure Left Binary where
	type Substructural Left Binary = Binary
	substructure empty_tree@(run . extract . run -> Nothing) = Store $ extract (run empty_tree) :*: (!) empty_tree
	substructure (run . extract . run -> Just tree) = lift . lift <$> sub @Left tree

instance Substructure Right Binary where
	type Substructural Right Binary = Binary
	substructure empty_tree@(run . extract . run -> Nothing) = Store $ extract (run empty_tree) :*: (!) empty_tree
	substructure (run . extract . run -> Just tree) = lift . lift <$> sub @Right tree

binary :: forall t a . (Traversable t, Chain a) => t a -> Binary a
binary struct = attached $ run @(State (Binary a)) % empty $ struct ->> modify @(Binary a) . insert' where

	insert' :: a -> Binary a -> Binary a
	insert' x (run -> Nothing) = lift . Construct x $ End
	insert' x tree@(run -> Just nonempty) = x <=> extract nonempty & order
		(tree & substitute @Left (insert' x)) tree (tree & substitute @Right (insert' x))

type instance Nonempty Binary = Construction Wye

instance Morphable (Into Binary) (Construction Wye) where
	type Morphing (Into Binary) (Construction Wye) = Binary
	morphing = lift . premorph

instance Focusable Root (Construction Wye) where
	type Focusing Root (Construction Wye) a = a
	focusing (extract -> Construct x xs) = Store $ x :*: Tag . Construct % xs

instance (forall a . Chain a) => Insertable (Construction Wye) where
	x += b = let change = lift . resolve (x +=) (Construct x End) . run in
		x <=> extract b & order (over (sub @Left) change $ b) b (over (sub @Right) change $ b)

instance Measurable Heighth (Construction Wye) where
	type Measural Heighth (Construction Wye) a = Denumerator
	measurement (deconstruct . extract -> End) = One
	measurement (deconstruct . extract -> Left lst) = One + measure @Heighth lst
	measurement (deconstruct . extract -> Right rst) = One + measure @Heighth rst
	measurement (deconstruct . extract -> Both lst rst) = One +
		let (lm :*: rm) = measure @Heighth lst :*: measure @Heighth rst
		in lm <=> rm & order rm lm lm

instance Substructure Left (Construction Wye) where
	type Substructural Left (Construction Wye) = Binary
	substructure empty_tree@(extract . run -> Construct _ End) =
		Store $ empty :*: (empty_tree !)
	substructure (extract . run -> Construct x (Left lst)) =
		Store $ lift lst :*: lift . Construct x . resolve Left End . run
	substructure (extract . run -> Construct x (Right rst)) =
		Store $ empty :*: lift . Construct x . resolve (Both % rst) (Right rst) . run
	substructure (extract . run -> Construct x (Both lst rst)) =
		Store $ lift lst :*: lift . Construct x . resolve (Both % rst) (Right rst) . run

instance Substructure Right (Construction Wye) where
	type Substructural Right (Construction Wye) = Binary
	substructure emtpy_tree@(extract . run -> Construct _ End) =
		Store $ empty :*: (emtpy_tree !)
	substructure (extract . run -> Construct x (Left lst)) =
		Store $ empty :*: lift . Construct x . resolve (Both lst) (Left lst) . run
	substructure (extract . run -> Construct x (Right rst)) =
		Store $ lift rst :*: lift . Construct x . resolve Right End . run
	substructure (extract . run -> Construct x (Both lst rst)) =
		 Store $ lift rst :*: lift . Construct x . resolve (Both lst) (Left lst) . run

data Biforked a = Top | Leftward a | Rightward a

instance Covariant Biforked where
	_ <$> Top = Top
	f <$> Leftward l = Leftward $ f l
	f <$> Rightward r = Rightward $ f r

type instance Zipper (Construction Wye) = T_U Covariant Covariant (:*:) (Construction Wye)
	((Biforked <:.> Construction Biforked) <:.> T_U Covariant Covariant (:*:) Identity (Maybe <:.> Construction Wye))

data Vertical a = Up a | Down a

instance Morphable (Rotate Up) (T_U Covariant Covariant (:*:) (Construction Wye) ((Biforked <:.> Construction Biforked) <:.> T_U Covariant Covariant (:*:) Identity (Maybe <:.> Construction Wye))) where
	type Morphing (Rotate Up) (T_U Covariant Covariant (:*:) (Construction Wye) ((Biforked <:.> Construction Biforked) <:.> T_U Covariant Covariant (:*:) Identity (Maybe <:.> Construction Wye)))
		= Maybe <:.> (T_U Covariant Covariant (:*:) (Construction Wye) ((Biforked <:.> Construction Biforked) <:.> T_U Covariant Covariant (:*:) Identity (Maybe <:.> Construction Wye)))
	morphing (run . premorph -> focused :*: TU (TU (Rightward (Construct (T_U (Identity parent :*: rest)) next)))) =
		lift $ twosome (Construct parent . resolve (Both focused) (Left focused) $ run rest) (TU $ TU next)
	morphing (run . premorph -> focused :*: TU (TU (Rightward (Construct (T_U (Identity parent :*: rest)) next)))) =
		lift $ twosome (Construct parent . resolve (Both % focused) (Right focused) $ run rest) (TU $ TU next)
	morphing (premorph -> T_U (_ :*: TU (TU Top))) = empty

instance Morphable (Rotate (Down Left)) (T_U Covariant Covariant (:*:) (Construction Wye) ((Biforked <:.> Construction Biforked) <:.> T_U Covariant Covariant (:*:) Identity (Maybe <:.> Construction Wye))) where
	type Morphing (Rotate (Down Left)) (T_U Covariant Covariant (:*:) (Construction Wye) ((Biforked <:.> Construction Biforked) <:.> T_U Covariant Covariant (:*:) Identity (Maybe <:.> Construction Wye)))
		= Maybe <:.> (T_U Covariant Covariant (:*:) (Construction Wye) ((Biforked <:.> Construction Biforked) <:.> T_U Covariant Covariant (:*:) Identity (Maybe <:.> Construction Wye)))
	morphing (run . premorph -> Construct x (Left lst) :*: TU (TU next)) =
		lift $ twosome lst . TU . TU . Leftward . Construct (twosome (Identity x) empty) $ next
	morphing (run . premorph -> Construct x (Both lst rst) :*: TU (TU next)) =
		lift $ twosome lst . TU . TU . Leftward . Construct (twosome (Identity x) $ lift rst) $ next
	morphing (run . premorph -> Construct _ (Right _) :*: _) = empty
	morphing (run . premorph -> Construct _ End :*: _) = empty

instance Morphable (Rotate (Down Right)) (T_U Covariant Covariant (:*:) (Construction Wye) ((Biforked <:.> Construction Biforked) <:.> T_U Covariant Covariant (:*:) Identity (Maybe <:.> Construction Wye))) where
	type Morphing (Rotate (Down Right)) (T_U Covariant Covariant (:*:) (Construction Wye) ((Biforked <:.> Construction Biforked) <:.> T_U Covariant Covariant (:*:) Identity (Maybe <:.> Construction Wye)))
		= Maybe <:.> (T_U Covariant Covariant (:*:) (Construction Wye) ((Biforked <:.> Construction Biforked) <:.> T_U Covariant Covariant (:*:) Identity (Maybe <:.> Construction Wye)))
	morphing (run . premorph -> Construct x (Right rst) :*: TU (TU next)) = lift $ twosome rst . TU . TU . Rightward . Construct (twosome (Identity x) empty) $ next
	morphing (run . premorph -> Construct x (Both lst rst) :*: TU (TU next)) = lift $ twosome rst . TU . TU . Rightward . Construct (twosome (Identity x) $ lift lst) $ next
	morphing (run . premorph -> Construct _ (Left _) :*: _) = empty
	morphing (run . premorph -> Construct _ End :*: _) = empty
