{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Structure.Modification.Tape where

import Pandora.Core.Impliable (Impliable (Arguments, imply))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), (<------))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--), (<-|---)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-), (<<---)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Paradigm.Algebraic.Exponential (type (<--), type (-->), (%))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)), type (<:*:>), (<:*:>))
import Pandora.Paradigm.Algebraic ((<-*-), (<-*--), (<-*---), (.-*-), extract, point)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Schemes.TT (TT (TT), type (<::>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged)
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Reverse (Reverse (Reverse))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, unite, (<~), (=#-))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable, Morph (Rotate), Vertical (Up, Down), Occurrence (All))
import Pandora.Paradigm.Structure.Interface.Zipper (Zippable)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substance, substructure), Segment (Root, Rest), sub)
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Inventory.Some.Optics (Lens, Convex, view, replace, mutate)

type Tape structure = Exactly <:*:> Reverse structure <:*:> structure

-- TODO: No overlapping, let's use wrappers instead
-- instance {-# OVERLAPS #-} Traversable (->) (->) t => Traversable (->) (->) (Tape t) where
-- 	f <<- z = (\ls x rs -> lift <------ x <:*:> ls <:*:> rs)
-- 		<-|--- f <<--- view <-- sub @Left <-- z
-- 		<-*--- f <<--- view <-- sub @Root <-- z
-- 		<-*--- f <<--- view <-- sub @Right <-- z

instance Covariant (->) (->) t => Impliable (Tape t a) where
	type Arguments (Tape t a) = a -> t a -> t a -> Tape t a
	imply focused left right = Exactly focused <:*:> Reverse left <:*:> right

instance Covariant (->) (->) t => Substructure Up (Tape t <::> Tape t) where
	type Substance Up (Tape t <::> Tape t) = t <::> Tape t
	substructure = P_Q_T <-- \x -> case run . run . lower <-- x of
		Exactly focused :*: T_U (Reverse d :*: u) ->
			Store <--- TT u :*: lift . TT . imply @(Tape t _) focused d . run

instance Covariant (->) (->) t => Substructure Down (Tape t <::> Tape t) where
	type Substance Down (Tape t <::> Tape t) = Reverse t <::> Tape t
	substructure = P_Q_T <-- \ii -> case run . run . lower <-- ii of
		Exactly focused :*: T_U (d :*: u) ->
			Store <--- TT d :*: lift . TT . (imply @(Tape t _) focused % u) . run . run

-- instance (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => Substructure (All Left) (Tape t <::> Tape t) where
-- 	type Substance (All Left) (Tape t <::> Tape t) = Tape t <::> Reverse t
-- 	substructure = P_Q_T <-- \source ->
-- 		let target = (view (sub @Left) <-|-) =#- lower source in
-- 		let updated new = replace % sub @Left <-|-- new <-*-- run <-- lower source in
-- 		Store <--- target :*: lift . (updated =#-)
--
-- instance (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => Substructure (All Right) (Tape t <::> Tape t) where
-- 	type Substance (All Right) (Tape t <::> Tape t) = Tape t <::> t
-- 	substructure = P_Q_T <-- \source ->
-- 		let target = (view (sub @Right) <-|-) =#- lower source in
-- 		let updated new = replace % sub @Right <-|-- new <-*-- run <-- lower source in
-- 			Store <--- target :*: lift . (updated =#-)
