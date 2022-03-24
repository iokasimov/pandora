{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Pandora.Paradigm.Structure.Interface.Zipper where

import Pandora.Core.Functor (type (>), type (<), type (:.), type (:::))
import Pandora.Core.Impliable (Impliable (Arguments, imply))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), (<------))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--), (<-|---)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-), (<<---)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Paradigm.Algebraic.Exponential (type (<--), type (-->), (%))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)), type (<:*:>), (<:*:>))
import Pandora.Paradigm.Algebraic ((<-*-), (<-*--), (<-*---), (.-*-), extract, point)
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable, Morph (Rotate))
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substance, substructure, sub), Segment (Root, Rest))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, unite, (<~), (=#-))

class Zippable (structure :: * -> *) where
	type Breadcrumbs structure :: * -> *

type Zipper (structure :: * -> *) = Tagged (Zippable structure) <:.> (Exactly <:*:> Breadcrumbs structure)

type Breadcrumbed structure t = (Zippable structure, Breadcrumbs structure ~ t)

instance {-# OVERLAPS #-} Semimonoidal (<--) (:*:) (:*:) t
	=> Semimonoidal (<--) (:*:) (:*:) (Exactly <:*:> t) where
	mult = Flip <-- \(T_U (Exactly (x :*: y) :*: xys)) ->
		let xs :*: ys = mult @(<--) <~ xys in
			(Exactly x <:*:> xs) :*: (Exactly y <:*:> ys)

instance {-# OVERLAPS #-} Semimonoidal (<--) (:*:) (:*:) t => Monoidal (<--) (-->) (:*:) (:*:) (Exactly <:*:> t) where
	unit _ = Flip <-- \(T_U (Exactly x :*: _)) -> Straight (\_ -> x)

instance Covariant (->) (->) t => Substructure Root (Tagged (Zippable structure) <:.> (Exactly <:*:> t)) where
	type Substance Root (Tagged (Zippable structure) <:.> (Exactly <:*:> t)) = Exactly
	substructure = P_Q_T <-- \source -> case lower . lower <-- source of
		T_U (Exactly x :*: xs) -> Store <--- Exactly x :*: lift . lift . (<:*:> xs)

type family Fastenable structure rs where
	Fastenable structure (r ::: rs) = (Morphable < Rotate r < structure, Fastenable structure rs)
	Fastenable structure r = Morphable < Rotate r < structure
