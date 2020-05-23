{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Stack (Stack, push, pop, filter, linearize) where

import Pandora.Core.Functor (type (:.), type (~>))
import Pandora.Core.Morphism ((&), (%))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Alternative ((<+>))
import Pandora.Pattern.Functor.Avoidable (empty)
import Pandora.Pattern.Functor.Pointable (point)
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Traversable (Traversable)
import Pandora.Pattern.Functor.Bindable ((>>=))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Primary.Object.Boolean ((?))
import Pandora.Paradigm.Primary.Functor.Delta (Delta ((:^:)))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Inventory.State (fold)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics (view)
import Pandora.Paradigm.Controlflow.Joint.Interpreted (run)
import Pandora.Paradigm.Controlflow.Joint.Schemes.TU (TU (TU))
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Zipper (Zipper, Tap (Tap))
import Pandora.Paradigm.Structure.Ability.Focusable (Focusable (Focus, top, singleton))

-- | Linear data structure that serves as a collection of elements
type Stack = TU Covariant Covariant Maybe (Construction Maybe)

instance Setoid a => Setoid (Stack a) where
	TU ls == TU rs = ls == rs

instance Semigroup (Stack a) where
	TU Nothing + TU ys = TU ys
	TU (Just (Construct x xs)) + TU ys = TU . Just . Construct x . run
		$ TU @Covariant @Covariant xs + TU @Covariant @Covariant ys

instance Monoid (Stack a) where
	zero = TU Nothing

instance Focusable Stack where
	type Focus Stack a = Maybe a
	top stack = Store $ (:*:) (extract <$> run stack) $ \case
		Just x -> stack & pop & push x
		Nothing -> pop stack
	singleton = TU . Just . Construct % Nothing

push :: a -> Stack a -> Stack a
push x (TU stack) = TU $ (Construct x . Just <$> stack) <+> (point . point) x

pop :: Stack ~> Stack
pop (TU stack) = TU $ stack >>= deconstruct

filter :: Predicate a -> Stack a -> Stack a
filter (Predicate p) = TU . fold empty
	(\now new -> p now ? Just (Construct now new) $ new)

-- | Transform any traversable structure into a stack
linearize :: Traversable t => t ~> Stack
linearize = TU . fold Nothing (\x -> Just . Construct x)

type instance Nonempty Stack = Construction Maybe

instance Focusable (Construction Maybe) where
	type Focus (Construction Maybe) a = a
	top stack = Store $ extract stack :*: Construct % deconstruct stack
	singleton = Construct % Nothing

type instance Zipper Stack = Tap (TU Covariant Covariant Delta Stack)

forward, backward :: Zipper Stack a -> Maybe (Zipper Stack a)
forward (Tap x (TU (bs :^: fs))) = Tap % (TU $ push x bs :^: pop fs) <$> view top fs
backward (Tap x (TU (bs :^: fs))) = Tap % (TU $ pop bs :^: push x fs) <$> view top bs
