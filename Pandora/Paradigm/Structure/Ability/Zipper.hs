{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Pandora.Paradigm.Structure.Ability.Zipper where

import Pandora.Core.Functor (type (>), type (<), type (:.), type (:::))
import Pandora.Core.Impliable (Impliable (Arguments, imply))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), (<------))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Paradigm.Algebraic.Exponential (type (<--), type (-->), (%))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)), type (<:*:>), (<:*:>))
import Pandora.Paradigm.Algebraic ((<-*-), (<-*--), extract, point)
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged)
import Pandora.Paradigm.Primary.Transformer.Reverse (Reverse (Reverse))
import Pandora.Paradigm.Schemes.TT (TT (TT), type (<::>))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable, Morph (Rotate), Vertical (Up, Down), Occurrence (All))
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substance, substructure), Segment (Root), sub)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (<~), (=#-))
import Pandora.Paradigm.Inventory.Ability.Gettable (get)
import Pandora.Paradigm.Inventory.Ability.Settable (set)
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Inventory.Some.Optics (Lens, Convex, view, replace, mutate)

class Zippable (structure :: * -> *) where
	type Breadcrumbs structure :: * -> *

type Zipper (structure :: * -> *) = Tagged Zippable <:.> (Exactly <:*:> Breadcrumbs structure)

type Breadcrumbed structure t = (Zippable structure, Breadcrumbs structure ~ t)

instance {-# OVERLAPS #-} Semimonoidal (<--) (:*:) (:*:) t
	=> Semimonoidal (<--) (:*:) (:*:) (Exactly <:*:> t) where
	mult = Flip <-- \(T_U (Exactly (x :*: y) :*: xys)) ->
		let xs :*: ys = mult @(<--) <~ xys in
			(Exactly x <:*:> xs) :*: (Exactly y <:*:> ys)

instance {-# OVERLAPS #-} Semimonoidal (<--) (:*:) (:*:) t => Monoidal (<--) (-->) (:*:) (:*:) (Exactly <:*:> t) where
	unit _ = Flip <-- \(T_U (Exactly x :*: _)) -> Straight (\_ -> x)

type family Fastenable structure rs where
	Fastenable structure (r ::: rs) = (Morphable < Rotate r < structure, Fastenable structure rs)
	Fastenable structure r = Morphable < Rotate r < structure

type Tape t = Tagged Zippable <:.> (Exactly <:*:> Reverse t <:*:> t)

instance Covariant (->) (->) t => Impliable (Tape t a) where
	type Arguments (Tape t a) = a -> t a -> t a -> Tape t a
	imply focused left right = lift <------ Exactly focused <:*:> Reverse left <:*:> right

-- TODO: Isn't too fragile to define such an instance without any hints about zippers?
instance Covariant (->) (->) t => Substructure Root (Tape t) where
	type Substance Root (Tape t) = Exactly
	substructure = P_Q_T <-- \zipper -> case run . lower <-- lower zipper of
		 Exactly x :*: xs -> Store <--- Exactly x :*: lift . lift . T_U . (:*: xs)

instance Covariant (->) (->) t => Substructure Left (Tape t) where
	type Substance Left (Tape t) = Reverse t
	substructure = P_Q_T <-- \zipper -> case run . lower <-- lower zipper of
		Exactly x :*: T_U (ls :*: rs) -> Store <--- ls :*: lift . (imply @(Tape t _) x % rs) . run

instance Covariant (->) (->) t => Substructure Right (Tape t) where
	type Substance Right (Tape t) = t
	substructure = P_Q_T <-- \zipper -> case run . lower <-- lower zipper of
		Exactly x :*: T_U (Reverse ls :*: rs) -> Store <--- rs :*: lift . imply @(Tape t _) x ls

instance Covariant (->) (->) t => Substructure Up (Tape t <::> Tape t) where
	type Substance Up (Tape t <::> Tape t) = t <::> Tape t
	substructure = P_Q_T <-- \x -> case run . lower . run . lower <-- x of
		Exactly focused :*: T_U (Reverse d :*: u) ->
			Store <--- TT u :*: lift . TT . imply @(Tape t _) focused d . run

instance Covariant (->) (->) t => Substructure Down (Tape t <::> Tape t) where
	type Substance Down (Tape t <::> Tape t) = Reverse t <::> Tape t
	substructure = P_Q_T <-- \ii -> case run . lower . run . lower <-- ii of
		Exactly focused :*: T_U (d :*: u) ->
			Store <--- TT d :*: lift . TT . (imply @(Tape t _) focused % u) . run . run

instance (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => Substructure (All Left) (Tape t <::> Tape t) where
	type Substance (All Left) (Tape t <::> Tape t) = Tape t <::> Reverse t
	substructure = P_Q_T <-- \source ->
		let target = (view (sub @Left) <-|-) =#- lower source in
		let updated new = replace % sub @Left <-|-- new <-*-- run <-- lower source in
		Store <--- target :*: lift . (updated =#-)

instance (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => Substructure (All Right) (Tape t <::> Tape t) where
	type Substance (All Right) (Tape t <::> Tape t) = Tape t <::> t
	substructure = P_Q_T <-- \source ->
		let target = (view (sub @Right) <-|-) =#- lower source in
		let updated new = replace % sub @Right <-|-- new <-*-- run <-- lower source in
			Store <--- target :*: lift . (updated =#-)
