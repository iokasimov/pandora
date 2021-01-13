{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Stack where

import Pandora.Core.Functor (type (:.), type (:=), type (|->))
import Pandora.Core.Morphism ((&), (%), (!))
import Pandora.Pattern ((.|..))
import Pandora.Pattern.Category ((.), ($), identity)
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Alternative ((<+>))
import Pandora.Pattern.Functor.Pointable (point)
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Traversable (Traversable)
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False), (?))
import Pandora.Paradigm.Primary.Object.Numerator (Numerator (Numerator))
import Pandora.Paradigm.Primary.Object.Denumerator (Denumerator (One))
import Pandora.Paradigm.Primary.Functor.Delta (Delta ((:^:)))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct, (.-+))
import Pandora.Paradigm.Primary.Transformer.Tap (Tap (Tap))
import Pandora.Paradigm.Inventory.State (State, fold)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics (view, (^.))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, unite)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Nullable (Nullable (null))
import Pandora.Paradigm.Structure.Ability.Zipper (Zipper)
import Pandora.Paradigm.Structure.Ability.Focusable (Focusable (Focusing, focusing), Location (Head), focus)
import Pandora.Paradigm.Structure.Ability.Insertable (Insertable (insert))
import Pandora.Paradigm.Structure.Ability.Measurable (Measurable (Measural, measurement), Scale (Length), measure)
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (reduce))
import Pandora.Paradigm.Structure.Ability.Rotatable (Rotatable (Rotational, rotation), rotate)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substructural, substructure), Command (Delete), Segment (Tail), sub)

-- | Linear data structure that serves as a collection of elements
type Stack = Maybe <:.> Construction Maybe

instance Setoid a => Setoid (Stack a) where
	TU ls == TU rs = ls == rs

instance Semigroup (Stack a) where
	TU Nothing + TU ys = TU ys
	TU (Just (Construct x xs)) + TU ys = lift . Construct x . run
		$ TU @Covariant @Covariant xs + TU @Covariant @Covariant ys

instance Monoid (Stack a) where
	zero = TU Nothing

instance Focusable Head Stack where
	type Focusing Head Stack a = Maybe a
	focusing (extract -> stack) = Store $ extract <$> run stack :*: \case
		Just x -> stack & view (sub @Tail) & insert x & Tag
		Nothing -> Tag $ sub @Tail ^. stack

instance Insertable Stack where
	insert x (run -> stack) = unite $ (Construct x . Just <$> stack) <+> (point . point) x

instance Measurable Length Stack where
	type Measural Length Stack a = Numerator
	measurement (run . extract -> Nothing) = zero
	measurement (run . extract -> Just xs) = Numerator $ measure @Length xs

instance Nullable Stack where
	null = Predicate $ \case { TU Nothing -> True ; _ -> False }

instance Substructure Tail Stack a where
	type Substructural Tail Stack a = Stack a
	substructure (run . extract -> Just ns) = point . unite . Just <$> sub @Tail ns
	substructure (run . extract -> Nothing) = Store $ unite Nothing :*: point . identity

delete :: Setoid a => a -> Stack a -> Stack a
delete _ (TU Nothing) = TU Nothing
delete x (TU (Just (Construct y ys))) = x == y ? TU ys
	$ lift . Construct y . run . delete x $ TU ys

filter :: forall a . Predicate a -> Stack a -> Stack a
filter (Predicate p) = TU . extract
	. run @(State (Maybe :. Nonempty Stack := a)) % Nothing
	. fold (\now new -> p now ? Just (Construct now new) $ new)

-- | Transform any traversable structure into a stack
linearize :: forall t a . Traversable t => t a -> Stack a
linearize = TU . extract . run @(State (Maybe :. Nonempty Stack := a)) % Nothing . fold (Just .|.. Construct)

type instance Nonempty Stack = Construction Maybe

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

instance Substructure Tail (Construction Maybe) a where
	type Substructural Tail (Construction Maybe) a = Stack a
	substructure (extract -> Construct x xs) = Store $ unite xs :*: point . Construct x . run

instance Setoid a => Substructure Delete (Construction Maybe) a where
	type Substructural Delete (Construction Maybe) a = a |-> Stack
	substructure (extract -> xs) = Store $ (\x -> delete x . unite $ Just xs) :*: (point xs !)

type instance Zipper Stack = Tap (Delta <:.> Stack)

instance {-# OVERLAPS #-} Extendable (Tap (Delta <:.> Stack)) where
	z =>> f = let move rtt = TU . deconstruct $ rtt .-+ z
		in f <$> Tap z (TU $ move (rotate @Left) :^: move (rotate @Right))

instance Rotatable Left (Tap (Delta <:.> Stack)) where
	type Rotational Left (Tap (Delta <:.> Stack)) a = Maybe :. Zipper Stack := a
	rotation (extract -> Tap x (TU (bs :^: fs))) = Tap % (TU $ sub @Tail ^. bs :^: insert x fs) <$> focus @Head ^. bs

instance Rotatable Right (Tap (Delta <:.> Stack)) where
	type Rotational Right (Tap (Delta <:.> Stack)) a = Maybe :. Zipper Stack := a
	rotation (extract -> Tap x (TU (bs :^: fs))) = Tap % (TU $ insert x bs :^: sub @Tail ^. fs) <$> focus @Head ^. fs

type instance Zipper (Construction Maybe) = Tap (Delta <:.> Construction Maybe)

instance Rotatable Left (Tap (Delta <:.> Construction Maybe)) where
	type Rotational Left (Tap (Delta <:.> Construction Maybe)) a = Maybe :. Zipper (Construction Maybe) := a
	rotation (extract -> Tap x (TU (bs :^: fs))) = Tap (extract bs) . TU . (:^: insert x fs) <$> deconstruct bs

instance Rotatable Right (Tap (Delta <:.> Construction Maybe)) where
	type Rotational Right (Tap (Delta <:.> Construction Maybe)) a = Maybe :. Zipper (Construction Maybe) := a
	rotation (extract -> Tap x (TU (bs :^: fs))) = Tap (extract fs) . TU . (insert x bs :^:) <$> deconstruct fs

instance Monotonic a (Maybe <:.> Construction Maybe := a) where
	reduce f r = reduce f r . run
