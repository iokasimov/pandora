{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Rose (Rose) where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Core.Morphism ((!), (%))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Avoidable (Avoidable (empty))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing), maybe)
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Structure.Stack (Stack)
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Focusable (Focusable (Focus, top, singleton))
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substructural, sub))

type Rose = Maybe <:.> Construction Stack

instance Focusable Rose where
	type Focus Rose a = Maybe a
	top (TU Nothing) = Store $ Nothing :*: TU . (<$>) (Construct % empty)
	top (TU (Just x)) = Store $ Just (extract x) :*: maybe
		(TU $ Just x) -- TODO: Nothing at top's lens - should it remove something?
		(TU . Just . Construct % deconstruct x)
	singleton = TU . Just . Construct % empty

instance Substructure Just Rose where
	type Substructural Just Rose a = Stack :. Construction Stack := a
	sub (TU Nothing) = Store $ Tag (TU Nothing) :*: (TU Nothing !)
	sub (TU (Just (Construct x xs))) = Store $ Tag xs :*: TU . Just . Construct x . extract

type instance Nonempty Rose = Construction Stack

instance Substructure Just (Construction Stack) where
	type Substructural Just (Construction Stack) a = Stack :. Construction Stack := a
	sub (Construct x xs) = Store $ Tag xs :*: Construct x . extract

instance Focusable (Construction Stack) where
	type Focus (Construction Stack) a = a
	top rose = Store $ extract rose :*: Construct % deconstruct rose
	singleton = Construct % empty
