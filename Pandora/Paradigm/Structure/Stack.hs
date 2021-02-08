{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Stack where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern ((.|..))
import Pandora.Pattern.Category ((.), ($), identity)
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Alternative ((<+>))
import Pandora.Pattern.Functor.Pointable (point)
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
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct, (.-+))
import Pandora.Paradigm.Primary.Transformer.Tap (Tap (Tap))
import Pandora.Paradigm.Inventory.State (State, fold)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics (view)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Nullable (Nullable (null))
import Pandora.Paradigm.Structure.Ability.Zipper (Zipper)
import Pandora.Paradigm.Structure.Ability.Focusable (Focusable (Focusing, focusing), Location (Head), focus)
import Pandora.Paradigm.Structure.Ability.Deletable (Deletable (delete))
import Pandora.Paradigm.Structure.Ability.Insertable (Insertable (insert))
import Pandora.Paradigm.Structure.Ability.Measurable (Measurable (Measural, measurement), Scale (Length), measure)
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (reduce))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Rotate), morph)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substructural, substructure), Segment (Tail), sub, subview)

-- | Linear data structure that serves as a collection of elements
type Stack = Maybe <:.> Construction Maybe

instance Setoid a => Setoid (Stack a) where
	TU ls == TU rs = ls == rs

instance Semigroup (Stack a) where
	TU Nothing + TU ys = TU ys
	TU (Just (Construct x xs)) + TU ys = lift . Construct x . run
		$ TU @Covariant @Covariant xs + TU @Covariant @Covariant ys

instance Monoid (Stack a) where
	zero = empty

instance Focusable Head Stack where
	type Focusing Head Stack a = Maybe a
	focusing (extract -> stack) = Store $ extract <$> run stack :*: \case
		Just x -> stack & subview @Tail & insert x & Tag
		Nothing -> stack & subview @Tail & Tag

instance Insertable Stack where
	insert x (run -> stack) = TU $ (Construct x . Just <$> stack) <+> (point . point) x

instance Measurable Length Stack where
	type Measural Length Stack a = Numerator
	measurement (run . extract -> Nothing) = zero
	measurement (run . extract -> Just xs) = Numerator $ measure @Length xs

instance Nullable Stack where
	null = Predicate $ \case { TU Nothing -> True ; _ -> False }

instance Substructure Tail Stack where
	type Substructural Tail Stack = Stack
	substructure (run . extract . run -> Just ns) = lift . lift <$> sub @Tail ns
	substructure (run . extract . run -> Nothing) = Store $ empty :*: lift . identity

instance Deletable Stack where
	delete _ (TU Nothing) = TU Nothing
	delete x (TU (Just (Construct y ys))) = x == y ? TU ys
		$ lift . Construct y . run . delete @Stack x $ TU ys

filter :: forall a . Predicate a -> Stack a -> Stack a
filter (Predicate p) = TU . extract
	. run @(State (Maybe :. Nonempty Stack := a)) % Nothing
	. fold (\now new -> p now ? Just (Construct now new) $ new)

-- | Transform any traversable structure into a stack
linearize :: forall t a . Traversable t => t a -> Stack a
linearize = TU . extract . run @(State (Maybe :. Nonempty Stack := a)) % Nothing . fold (Just .|.. Construct)

type instance Nonempty Stack = Construction Maybe

instance {-# OVERLAPS #-} Semigroup (Construction Maybe a) where
	Construct x Nothing + ys = Construct x $ Just ys
	Construct x (Just xs) + ys = Construct x . Just $ xs + ys

instance Morphable Stack (Construction Maybe) where
	type Morphing Stack (Construction Maybe) = Stack
	morphing = lift . extract . run

instance Focusable Head (Construction Maybe) where
	type Focusing Head (Construction Maybe) a = a
	focusing (extract -> stack) = Store $ extract stack :*: Tag . Construct % deconstruct stack

instance Insertable (Construction Maybe) where
	insert x = Construct x . Just

instance Measurable Length (Construction Maybe) where
	type Measural Length (Construction Maybe) a = Denumerator
	measurement (deconstruct . extract -> Nothing) = One
	measurement (deconstruct . extract -> Just xs) = One + measure @Length xs

instance Monotonic a (Construction Maybe a) where
	reduce f r ~(Construct x xs) = f x $ reduce f r xs

instance Substructure Tail (Construction Maybe) where
	type Substructural Tail (Construction Maybe) = Stack
	substructure (extract . run -> Construct x xs) =
		Store $ TU xs :*: lift . Construct x . run

type instance Zipper Stack = Tap ((:*:) <:.:> Stack)

instance {-# OVERLAPS #-} Extendable (Tap ((:*:) <:.:> Stack)) where
	z =>> f = let move rtt = TU . deconstruct $ rtt .-+ z
		in f <$> Tap z (T_U $ move (run . morph @(Rotate Left)) :*: move (run . morph @(Rotate Right)))

instance Morphable (Rotate Left) (Tap ((:*:) <:.:> Stack)) where
	type Morphing (Rotate Left) (Tap ((:*:) <:.:> Stack)) = Maybe <:.> Zipper Stack
	morphing (extract . run -> Tap x (T_U (bs :*: fs))) = TU
		$ Tap % (T_U $ subview @Tail bs :*: insert x fs) <$> view (focus @Head) bs

instance Morphable (Rotate Right) (Tap ((:*:) <:.:> Stack)) where
	type Morphing (Rotate Right) (Tap ((:*:) <:.:> Stack)) = Maybe <:.> Zipper Stack
	morphing (extract . run -> Tap x (T_U (bs :*: fs))) = TU
		$ Tap % (T_U $ insert x bs :*: subview @Tail fs) <$> view (focus @Head) fs

type instance Zipper (Construction Maybe) = Tap ((:*:) <:.:> Construction Maybe)

instance Morphable (Rotate Left) (Tap ((:*:) <:.:> Construction Maybe)) where
	type Morphing (Rotate Left) (Tap ((:*:) <:.:> Construction Maybe)) = Maybe <:.> Zipper (Construction Maybe)
	morphing (extract . run -> Tap x (T_U (bs :*: fs))) = TU
		$ Tap (extract bs) . T_U . (:*: insert x fs) <$> deconstruct bs

instance Morphable (Rotate Right) (Tap ((:*:) <:.:> Construction Maybe)) where
	type Morphing (Rotate Right) (Tap ((:*:) <:.:> Construction Maybe)) = Maybe <:.> Zipper (Construction Maybe)
	morphing (extract . run -> Tap x (T_U (bs :*: fs))) = TU
		$ Tap (extract fs) . T_U . (insert x bs :*:) <$> deconstruct fs

instance Monotonic a (Maybe <:.> Construction Maybe := a) where
	reduce f r = reduce f r . run
