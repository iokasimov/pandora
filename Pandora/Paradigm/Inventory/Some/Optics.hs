{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Inventory.Some.Optics where

import Pandora.Core.Impliable (Impliable (Arguments, imply))
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (Category (identity, (#)))
import Pandora.Pattern.Kernel (Kernel (constant))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Invariant (Invariant ((<!<)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Representable (Representable (Representation, (<#>), tabulate))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (run, (!)))
import Pandora.Paradigm.Inventory.Ability.Viewable (Viewable (Viewing, view_))
import Pandora.Paradigm.Inventory.Ability.Replaceable (Replaceable (Replacement, replace))
import Pandora.Paradigm.Inventory.Ability.Modifiable (Modifiable (Modification, modify))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (-->), (%))
import Pandora.Paradigm.Primary.Algebraic (Pointable, point, extract, (>-|-<-|-))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Paradigm.Primary.Object.Boolean ((?))
import Pandora.Paradigm.Inventory.Some.Store (Store (Store), position, look, retrofit)
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))
import Pandora.Paradigm.Structure.Ability.Monotonic (resolve)

infixl 2 #=@

type Lens = P_Q_T (->) Store

instance Invariant (Flip (Lens available) tgt) where
	f <!< g = \(Flip (P_Q_T lens)) -> Flip . P_Q_T ! (g :*: (f <-|-) >-|-<-|-) lens

type family Convex lens where
	Convex Lens = Lens Identity

instance Semigroupoid (Lens Identity) where
	(.) :: Convex Lens between target -> Convex Lens source between -> Convex Lens source target
	P_Q_T to . P_Q_T from = P_Q_T ! \source ->
		let (Identity between :*: bs) = run # from source in
		let (Identity target :*: tb) = run # to between in
		Store ! Identity target :*: bs . Identity . tb

instance Category (Lens Identity) where
	identity :: Convex Lens source source
	identity = imply @(Convex Lens _ _) identity ((%) constant)

instance Semimonoidal (-->) (:*:) (:*:) (Lens Identity source) where
	mult = Straight ! \(P_Q_T x :*: P_Q_T y) -> P_Q_T ! \source ->
		let Store (Identity xt :*: ixts) :*: Store (Identity yt :*: _) = x source :*: y source in
		Store ! Identity (xt :*: yt) :*: \(Identity (xt_ :*: yt_)) ->
			let modified = ixts (Identity xt_) in
			extract # run (y modified) # Identity yt_

instance Impliable (P_Q_T (->) Store Identity source target) where
	type Arguments (P_Q_T (->) Store Identity source target) =
		(source -> target) -> (source -> target -> source) -> Lens Identity source target
	imply getter setter = P_Q_T ! \source -> Store ! Identity # getter source :*: setter source . extract

type family Obscure lens where
	Obscure Lens = Lens Maybe

instance Impliable (P_Q_T (->) Store Maybe source target) where
	type Arguments (P_Q_T (->) Store Maybe source target) =
		(source -> Maybe target) -> (source -> Maybe target -> source) -> Lens Maybe source target
	imply getter setter = P_Q_T ! \source -> Store ! getter source :*: setter source

instance Semigroupoid (Lens Maybe) where
	(.) :: Obscure Lens between target -> Obscure Lens source between -> Obscure Lens source target
	P_Q_T to . P_Q_T from = P_Q_T ! \source -> case run # from source of
		(Nothing :*: _) -> Store ! Nothing :*: \_ -> source
		(Just between :*: mbs) -> case run # to between of
			(Nothing :*: _) -> Store ! Nothing :*: \_ -> source
			(Just target :*: mtb) -> Store ! Just target :*: mbs . Just . mtb

instance Category (Lens Maybe) where
	identity :: Obscure Lens source source
	identity = imply @(Obscure Lens _ _) # Just # resolve identity

-- Lens as natural transformation
type (#=@) source target available = forall a . Lens available (source a) (target a)

-- | Get focused target value
view :: Lens available source target -> source -> available target
view lens = position @_ @(Store _) . run lens

-- Replace focused target value with new value
set :: Lens available source target -> available target -> source -> source
set lens new = look new . run lens

-- | Modify focused target value
over :: Lens available source target -> (available target -> available target) -> source -> source
over lens f = extract . retrofit f . run lens

-- | Representable based lens
represent :: forall t a . (Representable t, Setoid (Representation t)) => Representation t -> Convex Lens (t a) a
represent r = imply @(Convex Lens (t a) a) (r <#>) (\source target -> tabulate ! \r' -> r' == r ? target ! r' <#> source)

class Lensic previous next where
	type Lensally previous next :: * -> *
	(>>>) :: Lens previous source between -> Lens next between target -> Lens (Lensally previous next) source target

instance Semigroupoid (Lens t) => Lensic t t where
	type Lensally t t = t
	x >>> y = y . x

instance Lensic Maybe Identity where
	type Lensally Maybe Identity = Maybe
	P_Q_T from >>> P_Q_T to = P_Q_T ! \source -> case run # from source of
		(Nothing :*: _) -> Store ! Nothing :*: \_ -> source
		(Just between :*: mbs) -> case run # to between of
			(Identity target :*: itb) -> Store ! Just target :*: \mt -> mbs ! itb . Identity <-|- mt

instance Lensic Identity Maybe where
	type Lensally Identity Maybe = Maybe
	P_Q_T from >>> P_Q_T to = P_Q_T ! \source -> case run # from source of
		(Identity between :*: ibs) -> case run # to between of
			(Just target :*: mtb) -> Store ! Just target :*: ibs . Identity . mtb
			(Nothing :*: _) -> Store ! Nothing :*: \_ -> source

instance Viewable (Lens Identity) where
	type instance Viewing (Lens Identity) source target = Lens Identity source target -> source -> target
	view_ lens source = extract # view lens source

instance Viewable (Lens Maybe) where
	type instance Viewing (Lens Maybe) source target = Lens Maybe source target -> source -> Maybe target
	view_ lens source = view lens source

instance Pointable t => Replaceable (Lens t) where
	type instance Replacement (Lens t) source target = target -> Lens t source target -> source -> source
	replace target lens source = set lens # point target # source

instance (Viewable (Lens t), Covariant (->) (->) t, Pointable t) => Modifiable (Lens t) where
	type instance Modification (Lens t) source target = (target -> target) -> Lens t source target -> source -> source
	modify f lens source = over lens (f <-|-) source
