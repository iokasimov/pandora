{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Some.List where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern ((.|..))
import Pandora.Pattern.Category ((.), ($), identity)
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Avoidable (empty)
import Pandora.Pattern.Functor.Traversable (Traversable)
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False), (?))
import Pandora.Paradigm.Primary.Object.Numerator (Numerator (Numerator))
import Pandora.Paradigm.Primary.Object.Denumerator (Denumerator (One))
import Pandora.Paradigm.Primary.Functor.Function ((%), (&))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:), twosome)
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct, (.-+))
import Pandora.Paradigm.Primary.Transformer.Tap (Tap (Tap))
import Pandora.Paradigm.Inventory.State (State, fold)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics (view)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (||=))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Nullable (Nullable (null))
import Pandora.Paradigm.Structure.Ability.Zipper (Zipper)
import Pandora.Paradigm.Structure.Ability.Focusable (Focusable (Focusing, focusing), Location (Head), focus)
import Pandora.Paradigm.Structure.Ability.Deletable (Deletable ((-=)))
import Pandora.Paradigm.Structure.Ability.Measurable (Measurable (Measural, measurement), Scale (Length), measure)
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (reduce, resolve))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Rotate, Into, Insert), premorph, rotate, insert)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substructural, substructure), Segment (Tail), sub, subview)
import Pandora.Paradigm.Structure.Interface.Stack (Stack (push, pop))

-- | Linear data structure that serves as a collection of elements
type List = Maybe <:.> Construction Maybe

instance Setoid a => Setoid (List a) where
	TU ls == TU rs = ls == rs

instance Semigroup (List a) where
	TU Nothing + TU ys = TU ys
	TU (Just (Construct x xs)) + TU ys = lift . Construct x . run
		$ TU @Covariant @Covariant xs + TU @Covariant @Covariant ys

instance Monoid (List a) where
	zero = empty

instance Stack List where
	push x = lift . Construct x . run
	pop xs = resolve deconstruct Nothing ||= xs

instance Morphable (Insert Left) List where
	type Morphing (Insert Left) List = T_U Covariant Covariant (->) Identity List
	morphing (premorph -> xs) = T_U $ \(Identity x) -> lift . Construct x . run $ xs

instance Focusable Head List where
	type Focusing Head List a = Maybe a
	focusing (extract -> stack) = Store $ extract <$> run stack :*: \case
		Just x -> stack & subview @Tail & insert @Left x & Tag
		Nothing -> stack & subview @Tail & Tag

instance Measurable Length List where
	type Measural Length List a = Numerator
	measurement (run . extract -> Nothing) = zero
	measurement (run . extract -> Just xs) = Numerator $ measure @Length xs

instance Nullable List where
	null = Predicate $ \case { TU Nothing -> True ; _ -> False }

instance Substructure Tail List where
	type Substructural Tail List = List
	substructure (run . extract . run -> Just ns) = lift . lift <$> sub @Tail ns
	substructure (run . extract . run -> Nothing) = Store $ empty :*: lift . identity

instance Deletable List where
	_ -= TU Nothing = TU Nothing
	x -= TU (Just (Construct y ys)) = x == y ? TU ys
		$ lift . Construct y . run . (-=) @List x $ TU ys

filter :: forall a . Predicate a -> List a -> List a
filter (Predicate p) = TU . extract
	. run @(State (Maybe :. Nonempty List := a)) % Nothing
	. fold (\now new -> p now ? Just (Construct now new) $ new)

-- | Transform any traversable structure into a stack
linearize :: forall t a . Traversable t => t a -> List a
linearize = TU . extract . run @(State (Maybe :. Nonempty List := a)) % Nothing . fold (Just .|.. Construct)

type instance Nonempty List = Construction Maybe

instance {-# OVERLAPS #-} Semigroup (Construction Maybe a) where
	Construct x Nothing + ys = Construct x $ Just ys
	Construct x (Just xs) + ys = Construct x . Just $ xs + ys

instance Morphable (Into List) (Construction Maybe) where
	type Morphing (Into List) (Construction Maybe) = List
	morphing = lift . premorph

instance Focusable Head (Construction Maybe) where
	type Focusing Head (Construction Maybe) a = a
	focusing (extract -> stack) = Store $ extract stack :*: Tag . Construct % deconstruct stack

instance Morphable (Insert Left) (Construction Maybe) where
	type Morphing (Insert Left) (Construction Maybe) = T_U Covariant Covariant (->) Identity (Construction Maybe)
	morphing (premorph -> xs) = T_U $ \(Identity x) -> Construct x $ Just xs

instance Measurable Length (Construction Maybe) where
	type Measural Length (Construction Maybe) a = Denumerator
	measurement (deconstruct . extract -> Nothing) = One
	measurement (deconstruct . extract -> Just xs) = One + measure @Length xs

instance Monotonic a (Construction Maybe a) where
	reduce f r ~(Construct x xs) = f x $ reduce f r xs

instance Substructure Tail (Construction Maybe) where
	type Substructural Tail (Construction Maybe) = List
	substructure (extract . run -> Construct x xs) =
		Store $ TU xs :*: lift . Construct x . run

type instance Zipper List = Tap (List <:.:> List := (:*:))

instance {-# OVERLAPS #-} Extendable (Tap (List <:.:> List := (:*:))) where
	z =>> f = let move rtt = TU . deconstruct $ rtt .-+ z
		in f <$> Tap z (twosome (move $ run . rotate @Left) (move $ run . rotate @Right))

instance Morphable (Rotate Left) (Tap (List <:.:> List := (:*:))) where
	type Morphing (Rotate Left) (Tap (List <:.:> List := (:*:))) = Maybe <:.> Zipper List
	morphing (premorph -> Tap x (T_U (bs :*: fs))) = TU
		$ Tap % twosome (subview @Tail bs) (insert @Left x fs) <$> view (focus @Head) bs

instance Morphable (Rotate Right) (Tap (List <:.:> List := (:*:))) where
	type Morphing (Rotate Right) (Tap (List <:.:> List := (:*:))) = Maybe <:.> Zipper List
	morphing (premorph -> Tap x (T_U (bs :*: fs))) = TU
		$ Tap % twosome (insert @Left x bs) (subview @Tail fs) <$> view (focus @Head) fs

type instance Zipper (Construction Maybe) = Tap (Construction Maybe <:.:> Construction Maybe := (:*:))

instance Morphable (Rotate Left) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) where
	type Morphing (Rotate Left) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) = Maybe <:.> Zipper (Construction Maybe)
	morphing (premorph -> Tap x (T_U (bs :*: fs))) = TU
		$ Tap (extract bs) . twosome % (insert @Left x fs) <$> deconstruct bs

instance Morphable (Rotate Right) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) where
	type Morphing (Rotate Right) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) = Maybe <:.> Zipper (Construction Maybe)
	morphing (premorph -> Tap x (T_U (bs :*: fs))) = TU
		$ Tap (extract fs) . twosome (insert @Left x bs) <$> deconstruct fs

instance Monotonic a (Maybe <:.> Construction Maybe := a) where
	reduce f r = reduce f r . run
