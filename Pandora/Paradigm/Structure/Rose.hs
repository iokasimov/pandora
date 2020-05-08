module Pandora.Paradigm.Structure.Rose (Rose) where

import Pandora.Core.Morphism ((&), (%), (!))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct))
import Pandora.Paradigm.Controlflow.Joint.Schemes.TU (TU (TU))
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Structure.Stack (Stack)
import Pandora.Paradigm.Structure.Variation.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Variation.Substructure (Substructure (Output, sub))

type Rose = TU Covariant Covariant Maybe (Construction Stack)

type instance Nonempty Rose = Construction Stack

instance Substructure Just Rose where
	type Output Just Rose a = Stack (Construction Stack a)
	sub (TU Nothing) = Store $ Tag (TU Nothing) :*: (TU Nothing !)
	sub (TU (Just (Construct x xs))) = Store $ Tag xs :*: (TU . Just . Construct x . extract)
