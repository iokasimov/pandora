{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Stack where

import Pandora.Core.Functor (type (~>))
import Pandora.Core.Morphism ((&), (%))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)), (.|..))
import Pandora.Pattern.Functor.Alternative ((<+>))
import Pandora.Pattern.Functor.Avoidable (empty)
import Pandora.Pattern.Functor.Pointable (point)
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Traversable (Traversable)
import Pandora.Pattern.Functor.Bindable ((>>=))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Primary.Object.Boolean ((?))
import Pandora.Paradigm.Primary.Functor.Delta (Delta ((:^:)))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Primary.Transformer.Tap (Tap (Tap))
import Pandora.Paradigm.Inventory.State (fold)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics ((^.))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Zipper (Zipper)
import Pandora.Paradigm.Structure.Ability.Focusable (Focusable (Focusing, focusing), Location (Head), focus)

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
	focusing (Tag stack) = Store $ extract <$> run stack :*: \case
		Just x -> stack & pop & push x & Tag
		Nothing -> Tag $ pop stack

push :: a -> Stack a -> Stack a
push x (TU stack) = TU $ (Construct x . Just <$> stack) <+> (point . point) x

pop :: Stack ~> Stack
pop (TU stack) = TU $ stack >>= deconstruct

delete :: Setoid a => a -> Stack a -> Stack a
delete _ (TU Nothing) = TU Nothing
delete x (TU (Just (Construct y ys))) = x == y ? TU ys
	$ lift . Construct x . run . delete x $ TU ys

filter :: Predicate a -> Stack a -> Stack a
filter (Predicate p) = TU . fold empty
	(\now new -> p now ? Just (Construct now new) $ new)

-- | Transform any traversable structure into a stack
linearize :: Traversable t => t ~> Stack
linearize = TU . fold Nothing (Just .|.. Construct)

type instance Nonempty Stack = Construction Maybe

instance Focusable Head (Construction Maybe) where
	type Focusing Head (Construction Maybe) a = a
	focusing (Tag stack) = Store $ extract stack :*: Tag . Construct % deconstruct stack

type instance Zipper Stack = Tap (Delta <:.> Stack)

instance Covariant (Delta <:.> Stack) where
	f <$> (TU (bs :^: fs)) = TU $ f <$> bs :^: f <$> fs

forward, backward :: Zipper Stack a -> Maybe (Zipper Stack a)
forward (Tap x (TU (bs :^: fs))) = Tap % (TU $ push x bs :^: pop fs) <$> focus @Head ^. fs
backward (Tap x (TU (bs :^: fs))) = Tap % (TU $ pop bs :^: push x fs) <$> focus @Head ^. bs
