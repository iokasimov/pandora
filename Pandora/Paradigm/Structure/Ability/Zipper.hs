{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pandora.Paradigm.Structure.Ability.Zipper where

import Pandora.Core.Functor (type (:=), type (:::))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Paradigm.Primary.Algebraic (extract)
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--), type (-->))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable, Morph (Rotate))
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Available, Substance, substructure), Segment (Root), sub)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (!))
import Pandora.Paradigm.Inventory.Store (Store (Store))

class Zippable (structure :: * -> *) where
	type Breadcrumbs structure :: * -> *

type Zipper (structure :: * -> *) = Identity <:.:> Breadcrumbs structure := (:*:)

type Breadcrumbed structure t = (Zippable structure, Breadcrumbs structure ~ t)

instance {-# OVERLAPS #-} Semimonoidal (<--) (:*:) (:*:) t => Semimonoidal (<--) (:*:) (:*:) (Identity <:.:> t := (:*:)) where
	mult = Flip $ \(T_U (Identity (x :*: y) :*: xys)) ->
		let xs :*: ys = mult @(<--) ! xys in
			T_U (Identity x :*: xs) :*: T_U (Identity y :*: ys)

instance {-# OVERLAPS #-} Semimonoidal (<--) (:*:) (:*:) t => Monoidal (<--) (-->) (:*:) (:*:) (Identity <:.:> t := (:*:)) where
	unit _ = Flip $ \(T_U (Identity x :*: _)) -> Straight (\_ -> x)

type family Fastenable structure rs where
	Fastenable structure (r ::: rs) = (Morphable (Rotate r) structure, Fastenable structure rs)
	Fastenable structure r = Morphable (Rotate r) structure

type Tape t = Identity <:.:> (t <:.:> t := (:*:)) := (:*:)

-- TODO: It's too fragile to define such an instance without any hints about zippers?
-- Should we wrap Zipper in Tagged Zippable?
instance Covariant (->) (->) t => Substructure Root (Tape t) where
	type Available Root (Tape t) = Identity
	type Substance Root (Tape t) = Identity
	substructure = P_Q_T $ \zipper -> case run # lower zipper of
		 Identity x :*: xs -> Store $ Identity (Identity x) :*: lift . T_U . (:*: xs) . extract

instance Covariant (->) (->) t => Substructure Left (Tape t) where
	type Available Left (Tape t) = Identity
	type Substance Left (Tape t) = t
	substructure = P_Q_T $ \zipper -> case run # lower zipper of
		Identity x :*: T_U (ls :*: rs) -> Store $ Identity ls :*: lift . T_U . (Identity x :*:) . T_U . (:*: rs) . extract

instance (Covariant (->) (->) t) => Substructure Right (Tape t) where
	type Available Right (Tape t) = Identity
	type Substance Right (Tape t) = t
	substructure = P_Q_T $ \zipper -> case run # lower zipper of
		Identity x :*: T_U (ls :*: rs) -> Store $ Identity rs :*: lift . T_U . (Identity x :*:) . T_U . (ls :*:) . extract
