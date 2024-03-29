{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Structure.Modification.Tape where

import Pandora.Core.Functor (type (>), type (<), type (>>>))
import Pandora.Core.Impliable (Impliable (Arguments, imply))
import Pandora.Core.Interpreted (run, (=#-), (<~~))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--)))
import Pandora.Pattern.Functor.Bindable (Bindable ((====<<)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal)
import Pandora.Pattern.Functor.Monoidal (Monoidal)
import Pandora.Pattern.Transformation.Liftable (lift)
import Pandora.Pattern.Transformation.Lowerable (lower)
import Pandora.Pattern.Operation.Exponential (type (-->), (%))
import Pandora.Pattern.Operation.Product ((:*:) ((:*:)), attached)
import Pandora.Paradigm.Algebraic.Functor ((<-*--), (-+), extract, void, until)
import Pandora.Paradigm.Algebraic (type (<:*:>), (<:*:>))
import Pandora.Paradigm.Schemes.TT (TT (TT), type (<::>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U))
import Pandora.Paradigm.Schemes.TUT (TUT (TUT))
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))
import Pandora.Paradigm.Primary.Auxiliary (Vertical (Up, Down), Horizontal (Left, Right))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just))
import Pandora.Paradigm.Primary.Transformer.Reverse (Reverse (Reverse))
import Pandora.Paradigm.Controlflow.Effect.Transformer ((:>) (TM), wrap)
import Pandora.Paradigm.Structure.Ability.Morphable (Occurrence (All))
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substance, substructure), Segment (Root, Rest), sub)
import Pandora.Paradigm.Structure.Ability.Slidable (Slidable (Sliding, slide))
import Pandora.Paradigm.Structure.Interface.Stack (Stack (Topping, push, pop))
import Pandora.Paradigm.Structure.Modification.Turnover (Turnover (Turnover))
import Pandora.Paradigm.Inventory.Some.State (State, change)
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Inventory.Some.Optics (view, replace, mutate, transwrap)
import Pandora.Paradigm.Inventory (zoom, overlook)

type Tape structure = Exactly <:*:> Reverse structure <:*:> structure

-- TODO: No overlapping, let's use wrappers instead
-- instance {-# OVERLAPS #-} Traversable (->) (->) t => Traversable (->) (->) (Tape t) where
-- 	f <-/- z = (\ls x rs -> lift <------ x <:*:> ls <:*:> rs)
-- 		<-|--- f <-/--- view <-- sub @Left <-- z
-- 		<-*--- f <-/--- view <-- sub @Root <-- z
-- 		<-*--- f <-/--- view <-- sub @Right <-- z

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

instance (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => Substructure (All Left) (Tape t <::> Tape t) where
	type Substance (All Left) (Tape t <::> Tape t) = Tape t <::> Reverse t
	substructure = P_Q_T <-- \source ->
		let target = (view (sub @Left) . view (sub @Rest) <-|-) =#- lower source in
		let updated new = (\trg src -> mutate (replace <-- trg <-- sub @Left) <-- sub @Rest <-- src) <-|-- new <-*-- run <-- lower source in
		Store <--- target :*: lift . (updated =#-)

instance (Covariant (->) (->) t, Semimonoidal (-->) (:*:) (:*:) t) => Substructure (All Right) (Tape t <::> Tape t) where
	type Substance (All Right) (Tape t <::> Tape t) = Tape t <::> t
	substructure = P_Q_T <-- \source ->
		let target = (view (sub @Right) . view (sub @Rest) <-|-) =#- lower source in
		let updated new = (\trg src -> mutate (replace <-- trg <-- sub @Right) <-- sub @Rest <-- src) <-|-- new <-*-- run <-- lower source in
		Store <--- target :*: lift . (updated =#-)

instance (Covariant (->) (->) structure, Bindable (->) (Topping structure), Monoidal (-->) (-->) (:*:) (:*:) (Topping structure), Stack structure) => Slidable Right (Tape structure) where
	type Sliding Right (Tape structure) = Topping structure
	slide :: forall e . State > Tape structure e :> Topping structure >>> ()
	slide = void . wrap . zoom @(Tape structure e) (sub @Rest)
		. zoom (sub @Left) . zoom transwrap . push @structure . extract
			====<< wrap . zoom @(Tape structure e) (sub @Root) . overlook . change . constant
				====<< lift ====<< wrap <---- zoom @(Tape structure e) <--- sub @Rest
					<--- zoom <-- sub @Right <-- pop @structure

instance (Covariant (->) (->) structure, Stack structure, Bindable (->) (Topping structure), Monoidal (-->) (-->) (:*:) (:*:) (Topping structure)) => Slidable Left (Tape structure) where
	type Sliding Left (Tape structure) = Topping structure
	slide :: forall e . State > Tape structure e :> Topping structure >>> ()
	slide = void . wrap . zoom @(Tape structure e) (sub @Rest)
		. zoom (sub @Right) . push . extract
			====<< wrap . zoom @(Tape structure e) (sub @Root) . overlook . change . constant
				====<< lift ====<< wrap <---- zoom @(Tape structure e) <--- sub @Rest
					<--- zoom <-- sub @Left <-- zoom transwrap pop

-- TODO: generalize this instance over direction, but for this we need a defintion of opposite derection
instance (Covariant (->) (->) structure, Stack structure, Topping structure ~ Maybe) => Slidable Left (Turnover > Tape structure) where
	type Sliding Left (Turnover > Tape structure) = Exactly
	slide :: forall e . State (Turnover < Tape structure < e) :> Exactly >>> ()
	slide = TM . TUT <-- \(Turnover tape) ->
		let Just updated = attached <-|- slide @Left -+ until (slide @Right) <~~ tape in
		Exactly <--- Turnover updated :*: ()

instance (Covariant (->) (->) structure, Stack structure, Topping structure ~ Maybe) => Slidable Right (Turnover > Tape structure) where
	type Sliding Right (Turnover > Tape structure) = Exactly
	slide :: forall e . State (Turnover < Tape structure < e) :> Exactly >>> ()
	slide = TM . TUT <-- \(Turnover tape) ->
		let Just updated = attached <-|- slide @Right -+ until (slide @Left) <~~ tape in
		Exactly <--- Turnover updated :*: ()
