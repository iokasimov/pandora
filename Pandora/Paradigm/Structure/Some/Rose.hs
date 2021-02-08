{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Some.Rose where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant (comap))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Primary.Functor.Function ((!), (%))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Structure.Ability.Focusable (Focusable (Focusing, focusing), Location (Root))
import Pandora.Paradigm.Structure.Ability.Monotonic (resolve)
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Nullable (Nullable (null))
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substructural, substructure))
import Pandora.Paradigm.Structure.Some.Stack (Stack)

type Rose = Maybe <:.> Construction Stack

instance Focusable Root Rose where
	type Focusing Root Rose a = Maybe a
	focusing (run . extract -> Nothing) = Store $ Nothing :*: Tag . TU . comap (Construct % empty)
	focusing (run . extract -> Just rose) = Store $ Just (extract rose)
		:*: Tag . resolve (lift . Construct % deconstruct rose) empty

instance Nullable Rose where
	null = Predicate $ \case { TU Nothing -> True ; _ -> False }

instance Substructure Just Rose where
	type Substructural Just Rose = Stack <:.> Construction Stack
	substructure (run . extract . run -> Nothing) =
		Store $ empty :*: (lift empty !)
	substructure (run . extract . run -> Just (Construct x xs)) =
		Store $ TU xs :*: lift . lift . Construct x . run

type instance Nonempty Rose = Construction Stack

instance Focusable Root (Construction Stack) where
	type Focusing Root (Construction Stack) a = a
	focusing (Tag rose) = Store $ extract rose :*: Tag . Construct % deconstruct rose

instance Substructure Just (Construction Stack) where
	type Substructural Just (Construction Stack) = Stack <:.> Construction Stack
	substructure (extract . run -> Construct x xs) = Store $ TU xs :*: lift . Construct x . run
