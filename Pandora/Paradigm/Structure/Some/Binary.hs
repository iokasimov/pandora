{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Some.Binary where

import Pandora.Core.Functor (type (:.), type (:=), type (:=>))
import Pandora.Pattern.Category (identity, (.), ($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), ($>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Avoidable (empty)
import Pandora.Pattern.Functor.Bindable ((>>=))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Paradigm.Primary ()
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Primary.Object.Ordering (order)
import Pandora.Paradigm.Primary.Object.Numerator (Numerator (Numerator, Zero))
import Pandora.Paradigm.Primary.Object.Denumerator (Denumerator (One))
import Pandora.Paradigm.Primary.Functor (Comparison)
import Pandora.Paradigm.Primary.Functor.Convergence (Convergence (Convergence))
import Pandora.Paradigm.Primary.Functor.Function ((%), (&))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:), attached, twosome)
import Pandora.Paradigm.Primary.Functor.Wye (Wye (End, Left, Right, Both))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Schemes (TU (TU), T_U (T_U), PQ_ (PQ_), type (<:.>), type (<:.:>))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics (over, view)
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Nullable (Nullable (null))
import Pandora.Paradigm.Structure.Ability.Focusable (Location (Root))
import Pandora.Paradigm.Structure.Ability.Measurable (Measurable (Measural, measurement), Scale (Heighth), measure)
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (resolve))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing)
	, Morph (Rotate, Into, Insert, Lookup, Vary, Key, Element), Vertical (Up, Down), morph, premorph, lookup)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substructural, substructure), sub)
import Pandora.Paradigm.Structure.Ability.Zipper (Zipper)
import Pandora.Paradigm.Structure.Modification.Prefixed (Prefixed (Prefixed))

type Binary = Maybe <:.> Construction Wye

rebalance :: Chain a => (Wye :. Construction Wye := a) -> Nonempty Binary a
rebalance (Both x y) = extract x <=> extract y & order
	# Construct (extract x) (Both # rebalance (deconstruct x) # rebalance (deconstruct y))
	# Construct (extract y) (Both # x # rebalance (deconstruct y))
	# Construct (extract x) (Both # rebalance (deconstruct x) # y)

instance Morphable Insert Binary where
	type Morphing Insert Binary = (Identity <:.:> Comparison := (:*:)) <:.:> Binary := (->)
	morphing (run . premorph -> Nothing) = T_U $ \(T_U (Identity x :*: _)) -> lift $ leaf x
	morphing (run . premorph -> Just ne) = T_U $ \(T_U (Identity x :*: Convergence f)) ->
		let continue xs = run # morph @Insert xs $ twosome # Identity x # Convergence f
		in lift $ f x # extract ne & order # ne # over (sub @Left) continue ne # over (sub @Right) continue ne

instance Measurable Heighth Binary where
	type Measural Heighth Binary a = Numerator
	measurement (run . extract -> Just bt) = Numerator $ measure @Heighth bt
	measurement (run . extract -> Nothing) = Zero

instance Nullable Binary where
	null = Predicate $ \case { TU Nothing -> True ; _ -> False }

instance Substructure Left Binary where
	type Substructural Left Binary = Binary
	substructure = PQ_ $ \bintree -> case run . extract . run # bintree of
		Nothing -> Store $ empty :*: lift . identity
		Just tree -> lift . lift <$> run (sub @Left) tree

instance Substructure Right Binary where
	type Substructural Right Binary = Binary
	substructure = PQ_ $ \bintree -> case run . extract . run # bintree of
		Nothing -> Store $ empty :*: lift . identity
		Just tree -> lift . lift <$> run (sub @Right) tree

type instance Nonempty Binary = Construction Wye

instance Morphable (Into Binary) (Construction Wye) where
	type Morphing (Into Binary) (Construction Wye) = Binary
	morphing = lift . premorph

instance Morphable Insert (Construction Wye) where
	type Morphing Insert (Construction Wye) = (Identity <:.:> Comparison := (:*:)) <:.:> Construction Wye := (->)
	morphing (premorph -> nonempty_list) = T_U $ \(T_U (Identity x :*: Convergence f)) ->
		let continue xs = run # morph @Insert @(Nonempty Binary) xs $ twosome # Identity x # Convergence f in
		let change = lift . resolve continue (leaf x) . run in
		order # nonempty_list
			# over (sub @Left) change nonempty_list
			# over (sub @Right) change nonempty_list
			# f x (extract nonempty_list)

instance Measurable Heighth (Construction Wye) where
	type Measural Heighth (Construction Wye) a = Denumerator
	measurement (deconstruct . extract -> End) = One
	measurement (deconstruct . extract -> Left lst) = One + measure @Heighth lst
	measurement (deconstruct . extract -> Right rst) = One + measure @Heighth rst
	measurement (deconstruct . extract -> Both lst rst) = One +
		let (lm :*: rm) = measure @Heighth lst :*: measure @Heighth rst
		in lm <=> rm & order lm rm lm

instance Substructure Root (Construction Wye) where
	type Substructural Root (Construction Wye) = Identity
	substructure = PQ_ $ \bintree -> case lower bintree of
		Construct x xs -> Store $ Identity x :*: lift . (Construct % xs) . extract

instance Substructure Left (Construction Wye) where
	type Substructural Left (Construction Wye) = Binary
	substructure = PQ_ $ \bintree -> case extract # run bintree of
		Construct x End -> Store $ empty :*: lift . resolve (Construct x . Left) (leaf x) . run
		Construct x (Left lst) -> Store $ lift lst :*: lift . Construct x . resolve Left End . run
		Construct x (Right rst) -> Store $ empty :*: lift . Construct x . resolve (Both % rst) (Right rst) . run
		Construct x (Both lst rst) -> Store $ lift lst :*: lift . Construct x . resolve (Both % rst) (Right rst) . run

instance Substructure Right (Construction Wye) where
	type Substructural Right (Construction Wye) = Binary
	substructure = PQ_ $ \bintree -> case extract # run bintree of
		Construct x End -> Store $ empty :*: lift . resolve (Construct x . Right) (leaf x) . run
		Construct x (Left lst) -> Store $ empty :*: lift . Construct x . resolve (Both lst) (Left lst) . run
		Construct x (Right rst) -> Store $ lift rst :*: lift . Construct x . resolve Right End . run
		Construct x (Both lst rst) -> Store $ lift rst :*: lift . Construct x . resolve (Both lst) (Left lst) . run

instance Chain k => Morphable (Lookup Key) (Prefixed Binary k) where
	type Morphing (Lookup Key) (Prefixed Binary k) = (->) k <:.> Maybe
	morphing (run . run . premorph -> Nothing) = lift Nothing
	morphing (run . run . premorph -> Just tree) = TU $ \key ->
		let root = extract tree in key <=> attached root & order # Just (extract root)
			# lookup @Key key (Prefixed $ view # sub @Left # tree)
			# lookup @Key key (Prefixed $ view # sub @Right # tree)

instance Chain key => Morphable (Lookup Key) (Prefixed (Construction Wye) key) where
	type Morphing (Lookup Key) (Prefixed (Construction Wye) key) = (->) key <:.> Maybe
	morphing (run . premorph -> Construct x xs) = TU $ \key ->
		key <=> attached x & order (Just # extract x)
			(view # sub @Left # xs >>= lookup @Key key . Prefixed)
			(view # sub @Right # xs >>= lookup @Key key . Prefixed)

instance Chain k => Morphable (Vary Element) (Prefixed Binary k) where
	type Morphing (Vary Element) (Prefixed Binary k) = (Product k <:.> Identity) <:.:> Prefixed Binary k := (->)
	morphing (run . run . premorph -> Nothing) = T_U $ \(TU (key :*: Identity value)) -> Prefixed . lift . leaf $ key :*: value
	morphing (run . run . premorph -> Just tree) = T_U $ \(TU (key :*: Identity value)) ->
		let continue xs = run $ run # morph @(Vary Element) (Prefixed xs) # TU (key :*: Identity value)
		in let root = extract tree in Prefixed . lift $ key <=> attached root & order
			# over (sub @Root) (($> value) <$>) tree # over (sub @Left) continue tree # over (sub @Right) continue tree

data Biforked a = Top | Leftward a | Rightward a

instance Covariant Biforked where
	_ <$> Top = Top
	f <$> Leftward l = Leftward $ f l
	f <$> Rightward r = Rightward $ f r

type Bifurcation = Biforked <:.> Construction Biforked

type Bicursor = Identity <:.:> Binary := (:*:)

type instance Zipper (Construction Wye) = Construction Wye <:.:> Bifurcation <:.> Bicursor := (:*:)

instance Morphable (Rotate Up) (Construction Wye <:.:> Bifurcation <:.> Bicursor := (:*:)) where
	type Morphing (Rotate Up) (Construction Wye <:.:> Bifurcation <:.> Bicursor := (:*:))
		= Maybe <:.> (Construction Wye <:.:> Bifurcation <:.> Bicursor := (:*:))
	morphing (run . premorph -> focused :*: TU (TU (Rightward (Construct (T_U (Identity parent :*: rest)) next)))) =
		lift $ twosome # Construct parent (resolve # Both focused # Left focused # run rest) # TU (TU next)
	morphing (run . premorph -> focused :*: TU (TU (Rightward (Construct (T_U (Identity parent :*: rest)) next)))) =
		lift $ twosome # Construct parent (resolve # Both % focused # Right focused # run rest) # TU (TU next)
	morphing (premorph -> T_U (_ :*: TU (TU Top))) = empty

instance Morphable (Rotate (Down Left)) (Construction Wye <:.:> Bifurcation <:.> Bicursor := (:*:)) where
	type Morphing (Rotate (Down Left)) (Construction Wye <:.:> Bifurcation <:.> Bicursor := (:*:))
		= Maybe <:.> (Construction Wye <:.:> Bifurcation <:.> Bicursor := (:*:))
	morphing (run . premorph -> Construct x (Left lst) :*: TU (TU next)) =
		lift . twosome lst . TU . TU . Leftward $ Construct # twosome (Identity x) empty # next
	morphing (run . premorph -> Construct x (Both lst rst) :*: TU (TU next)) =
		lift . twosome lst . TU . TU . Leftward $ Construct # twosome (Identity x) (lift rst) # next
	morphing (run . premorph -> Construct _ (Right _) :*: _) = empty
	morphing (run . premorph -> Construct _ End :*: _) = empty

instance Morphable (Rotate (Down Right)) (Construction Wye <:.:> Bifurcation <:.> Bicursor := (:*:)) where
	type Morphing (Rotate (Down Right)) (Construction Wye <:.:> Bifurcation <:.> Bicursor := (:*:))
		= Maybe <:.> (Construction Wye <:.:> Bifurcation <:.> Bicursor := (:*:))
	morphing (run . premorph -> Construct x (Right rst) :*: TU (TU next)) =
		lift . twosome rst . TU . TU . Rightward $ Construct # twosome (Identity x) empty # next
	morphing (run . premorph -> Construct x (Both lst rst) :*: TU (TU next)) =
		lift . twosome rst . TU . TU . Rightward $ Construct # twosome (Identity x) (lift lst) # next
	morphing (run . premorph -> Construct _ (Left _) :*: _) = empty
	morphing (run . premorph -> Construct _ End :*: _) = empty

leaf :: a :=> Nonempty Binary
leaf x = Construct x End
