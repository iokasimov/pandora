{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Inventory.Some.Optics where

import Pandora.Core.Functor (type (<))
import Pandora.Core.Impliable (Impliable (Arguments, imply))
import Pandora.Core.Interpreted (Interpreted (Primary, run, unite, (<~)))
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (Category (identity, (<--), (<---), (<----), (<-----), (<-------)))
import Pandora.Pattern.Kernel (Kernel (constant))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|-|-)))
import Pandora.Pattern.Functor.Invariant (Invariant ((<!<)))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (mult))
import Pandora.Pattern.Functor.Representable (Representable (Representation, (<#>), tabulate))
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Transformer.Lowerable (Lowerable (lower))
import Pandora.Pattern.Object.Setoid (Setoid ((?=)))
import Pandora.Paradigm.Inventory.Ability.Gettable (Gettable (Getting, get))
import Pandora.Paradigm.Inventory.Ability.Settable (Settable (Setting, set))
import Pandora.Paradigm.Inventory.Ability.Modifiable (Modifiable (Modification, modify))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Algebraic.Exponential (type (-->), (%))
import Pandora.Paradigm.Algebraic (Pointable, point, extract, (>-||---))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Pattern.Morphism.Straight (Straight (Straight))
import Pandora.Paradigm.Inventory.Some.Store (Store (Store), position, look, retrofit)
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))
import Pandora.Paradigm.Structure.Ability.Monotonic (resolve)

infixl 2 #=@

type Lens = P_Q_T (->) Store

instance Invariant (Flip (Lens available) tgt) where
	f <!< g = \(Flip (P_Q_T lens)) -> Flip . P_Q_T <------- g >-||--- f <-|-|- lens

type family Convex lens where
	Convex Lens = Lens Exactly

instance Semigroupoid (Lens Exactly) where
	(.) :: Convex Lens between target -> Convex Lens source between -> Convex Lens source target
	P_Q_T to . P_Q_T from = P_Q_T <-- \source ->
		let (Exactly between :*: bs) = run <-- from source in
		let (Exactly target :*: tb) = run <-- to between in
		Store <--- Exactly target :*: bs . Exactly . tb

instance Category (Lens Exactly) where
	identity :: Convex Lens source source
	identity = imply @(Convex Lens _ _) identity ((%) constant)

instance Semimonoidal (-->) (:*:) (:*:) (Lens Exactly source) where
	mult = Straight <-- \(P_Q_T x :*: P_Q_T y) -> P_Q_T <-- \source ->
		let Store (Exactly xt :*: ixts) :*: Store (Exactly yt :*: _) = x source :*: y source in
		Store <--- Exactly (xt :*: yt) :*: \(Exactly (xt_ :*: yt_)) ->
			let modified = ixts <-- Exactly xt_ in
			extract <--- run <-- y modified <--- Exactly yt_

instance Impliable (P_Q_T (->) Store Exactly source target) where
	type Arguments (P_Q_T (->) Store Exactly source target) =
		(source -> target) -> (source -> target -> source) -> Lens Exactly source target
	imply getter setter = P_Q_T <-- \source -> Store <--- (Exactly <-- getter source) :*: setter source . extract

type family Obscure lens where
	Obscure Lens = Lens Maybe

instance Impliable (P_Q_T (->) Store Maybe source target) where
	type Arguments (P_Q_T (->) Store Maybe source target) =
		(source -> Maybe target) -> (source -> Maybe target -> source) -> Lens Maybe source target
	imply getter setter = P_Q_T <-- \source -> Store <--- getter source :*: setter source

instance Semigroupoid (Lens Maybe) where
	(.) :: Obscure Lens between target -> Obscure Lens source between -> Obscure Lens source target
	P_Q_T to . P_Q_T from = P_Q_T <-- \source -> case run <-- from source of
		Nothing :*: _ -> Store <--- Nothing :*: \_ -> source
		Just between :*: mbs -> case run <-- to between of
			Nothing :*: _ -> Store <--- Nothing :*: \_ -> source
			Just target :*: mtb -> Store <--- Just target :*: mbs . Just . mtb

instance Category (Lens Maybe) where
	identity :: Obscure Lens source source
	identity = imply @(Obscure Lens _ _) <-- Just <-- resolve identity

-- Lens as natural transformation
type (#=@) source target available = forall a . Lens available (source a) (target a)

type (@>>>) source target = forall a . Lens target (source a) a

-- | Representable based lens
represent :: forall t a . (Representable t, Setoid (Representation t)) => Representation t -> Convex Lens (t a) a
represent r = imply @(Convex Lens (t a) a) (r <#>) (\source target -> tabulate <-- \r' -> r' ?= r <----- target <----- r' <#> source)

class Lensic previous next where
	type Lensally previous next :: * -> *
	(>>>) :: Lens previous source between -> Lens next between target -> Lens (Lensally previous next) source target

instance Semigroupoid (Lens t) => Lensic t t where
	type Lensally t t = t
	x >>> y = y . x

instance Lensic Maybe Exactly where
	type Lensally Maybe Exactly = Maybe
	P_Q_T from >>> P_Q_T to = P_Q_T <-- \source -> case run <-- from source of
		Nothing :*: _ -> Store <--- Nothing :*: \_ -> source
		Just between :*: mbs -> case run <-- to between of
			Exactly target :*: itb -> Store <--- Just target :*: \mt -> mbs <---- itb . Exactly <-|- mt

instance Lensic Exactly Maybe where
	type Lensally Exactly Maybe = Maybe
	P_Q_T from >>> P_Q_T to = P_Q_T <-- \source -> case run <-- from source of
		Exactly between :*: ibs -> case run <-- to between of
			Just target :*: mtb -> Store <--- Just target :*: ibs . Exactly . mtb
			Nothing :*: _ -> Store <--- Nothing :*: constant source

instance Gettable (Lens Exactly) where
	type instance Getting (Lens Exactly) source target = Lens Exactly source target -> source -> target
	get lens source = extract @Exactly . position @_ @(Store _) <-- lens <~ source

instance Gettable (Lens Maybe) where
	type instance Getting (Lens Maybe) source target = Lens Maybe source target -> source -> Maybe target
	get lens source = position @_ @(Store _) <-- lens <~ source

instance Pointable t => Settable (Lens t) where
	type instance Setting (Lens t) source target = target -> Lens t source target -> source -> source
	set new lens source = look @(t _) <-- point new <-- lens <~ source

instance (Gettable (Lens t), Covariant (->) (->) t, Pointable t) => Modifiable (Lens t) where
	type instance Modification (Lens t) source target = (target -> target) -> Lens t source target -> source -> source
	modify f lens source = extract . retrofit (f <-|-) <-- lens <~ source

view :: Lens i source target -> source -> i target
view lens source = position @_ @(Store _) <-- lens <~ source

replace :: forall i source target . i target -> Lens i source target -> source -> source
replace new lens source = look @(i _) <-- new <-- lens <~ source

mutate :: (i target -> i target) -> Lens i source target -> source -> source
mutate mut lens source = extract . retrofit mut <-- lens <~ source

transwrap :: (Covariant (->) (->) u, Liftable (->) t, Lowerable (->) t) => Lens u < t u e < e
transwrap = P_Q_T <-- \origin -> Store <--- lower origin :*: lift

primary :: Interpreted (->) t => Lens Exactly < t a < Primary t a
primary = P_Q_T <-- \origin -> Store <--- (Exactly <-- run origin) :*: unite . extract
