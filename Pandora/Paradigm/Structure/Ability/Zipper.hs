{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pandora.Paradigm.Structure.Ability.Zipper where

import Pandora.Core.Functor (type (:=), type (:::))
import Pandora.Core.Impliable (Impliable (Arguments, imply))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Paradigm.Primary.Algebraic (extract)
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--), type (-->), (%))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic ((<-*-))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Reverse (Reverse (Reverse))
import Pandora.Paradigm.Primary (twosome)
import Pandora.Paradigm.Schemes.TT (TT (TT), type (<::>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable, Morph (Rotate), Vertical (Up, Down))
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Available, Substance, substructure), Segment (Root), sub)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (!), (||=))
import Pandora.Paradigm.Inventory.Ability.Gettable (get)
import Pandora.Paradigm.Inventory.Ability.Settable (set)
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Inventory.Some.Optics (Lens, Convex)

class Zippable (structure :: * -> *) where
	type Breadcrumbs structure :: * -> *

type Zipper (structure :: * -> *) = Exactly <:.:> Breadcrumbs structure := (:*:)

type Breadcrumbed structure t = (Zippable structure, Breadcrumbs structure ~ t)

instance {-# OVERLAPS #-} Semimonoidal (<--) (:*:) (:*:) t => Semimonoidal (<--) (:*:) (:*:) (Exactly <:.:> t := (:*:)) where
	mult = Flip ! \(T_U (Exactly (x :*: y) :*: xys)) ->
		let xs :*: ys = mult @(<--) ! xys in
			T_U (Exactly x :*: xs) :*: T_U (Exactly y :*: ys)

instance {-# OVERLAPS #-} Semimonoidal (<--) (:*:) (:*:) t => Monoidal (<--) (-->) (:*:) (:*:) (Exactly <:.:> t := (:*:)) where
	unit _ = Flip ! \(T_U (Exactly x :*: _)) -> Straight (\_ -> x)

type family Fastenable structure rs where
	Fastenable structure (r ::: rs) = (Morphable (Rotate r) structure, Fastenable structure rs)
	Fastenable structure r = Morphable (Rotate r) structure

type Tape t = Exactly <:.:> (Reverse t <:.:> t := (:*:)) := (:*:)

instance Impliable (Tape t a) where
	type Arguments (Tape t a) = a -> t a -> t a -> Tape t a
	imply focused left right = twosome # Exactly focused ! twosome # Reverse left # right

-- TODO: It's too fragile to define such an instance without any hints about zippers?
-- Should we wrap Zipper in Tagged Zippable?
instance Covariant (->) (->) t => Substructure Root (Tape t) where
	type Available Root (Tape t) = Exactly
	type Substance Root (Tape t) = Exactly
	substructure = P_Q_T ! \zipper -> case run # lower zipper of
		 Exactly x :*: xs -> Store ! Exactly (Exactly x) :*: lift . T_U . (:*: xs) . extract

instance Covariant (->) (->) t => Substructure Left (Tape t) where
	type Available Left (Tape t) = Exactly
	type Substance Left (Tape t) = Reverse t
	substructure = P_Q_T ! \zipper -> case run # lower zipper of
		Exactly x :*: T_U (ls :*: rs) -> Store ! Exactly ls :*: lift . (imply @(Tape t _) x % rs) . run . extract
		-- Exactly x :*: T_U (ls :*: rs) -> Store ! Exactly ls :*: lift . T_U . (Exactly x :*:) . T_U . (:*: rs) . extract

instance Covariant (->) (->) t => Substructure Right (Tape t) where
	type Available Right (Tape t) = Exactly
	type Substance Right (Tape t) = t
	substructure = P_Q_T ! \zipper -> case run # lower zipper of
		Exactly x :*: T_U (Reverse ls :*: rs) -> Store ! Exactly rs :*: lift . imply @(Tape t _) x ls . extract

instance Covariant (->) (->) t => Substructure Up (Tape t <::> Tape t) where
	type Available Up (Tape t <::> Tape t) = Exactly
	type Substance Up (Tape t <::> Tape t) = t <::> Tape t
	substructure = P_Q_T ! \x -> case run . run . extract . run # x of
		Exactly focused :*: T_U (Reverse d :*: u) ->
			Store ! Exactly (TT u) :*: lift . TT . imply @(Tape t _) focused d . run . extract

instance Covariant (->) (->) t => Substructure Down (Tape t <::> Tape t) where
	type Available Down (Tape t <::> Tape t) = Exactly
	type Substance Down (Tape t <::> Tape t) = Reverse t <::> Tape t
	substructure = P_Q_T ! \ii -> case run . run . extract . run # ii of
		Exactly focused :*: T_U (d :*: u) ->
			Store ! Exactly (TT d) :*: lift . TT . (imply @(Tape t _) focused % u) . run . run . extract

instance (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => Substructure Left (Tape t <::> Tape t) where
	type Available Left (Tape t <::> Tape t) = Exactly
	type Substance Left (Tape t <::> Tape t) = Tape t <::> Reverse t
	substructure = P_Q_T ! \ii ->
		let target = (get @(Convex Lens) (sub @Left) <-|-) ||= (lower ii) in
		let updated new = (set @(Convex Lens) % sub @Left) <-|- new <-*- run (lower ii) in
		Store ! Exactly target :*: lift . (updated ||=) . extract

instance (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => Substructure Right (Tape t <::> Tape t) where
	type Available Right (Tape t <::> Tape t) = Exactly
	type Substance Right (Tape t <::> Tape t) = Tape t <::> t
	substructure = P_Q_T ! \ii ->
		let target = (get @(Convex Lens) (sub @Right) <-|-) ||= lower ii in
		let updated new = (set @(Convex Lens) % sub @Right) <-|- new <-*- run (lower ii) in
		Store ! Exactly target :*: lift . (updated ||=) . extract
