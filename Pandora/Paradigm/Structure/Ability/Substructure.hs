{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Structure.Ability.Substructure where

import Pandora.Core.Functor (type (>))
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (identity, (<--), (<---), (<-----))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (=#-))
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Inventory.Some.Optics (Lens, Convex, type (#=@), type (@>>>))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)), type (<:*:>))
import Pandora.Paradigm.Primary.Algebraic ((>-|-<-|-))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly)
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged)
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Schemes.TU (type (<:.>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U))
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))

data Segment a = Root a | Tail a

type Substructured segment source target = (Substructure segment source, Substance segment source ~ target)

class Substructure segment (structure :: * -> *) where
	type Substance segment structure :: * -> *
	substructure :: (Tagged segment <:.> structure) @>>> Substance segment structure

	sub :: (Covariant (->) (->) structure) => structure @>>> Substance segment structure
	sub = ((lift :*: (lower @(->) <-|-)) >-|-<-|-) =#- substructure @segment @structure

-- TODO: generalize `available` and then rename to `singleton`
-- The main problem is that we should handle (Maybe target -> sourse)
-- For Convex Lens: we can ignore Exactly cause we can wrap/unwrap its value
-- For Obscure Lens: if we got nothing -> nothing should change
--only :: forall segment structure element . (Covariant (->) (->) structure, Substructured segment structure Exactly Exactly) => Convex Lens (structure element) element
--only = inner . ((sub @segment) :: Convex Lens (structure element) (Exactly element)) where

--	inner :: Convex Lens (Exactly element) element
--	inner = P_Q_T <-- \x -> Store <--- x :*: identity

instance (Covariant (->) (->) t, Covariant (->) (->) u) => Substructure Left (t <:*:> u) where
	type Substance Left (t <:*:> u) = t
	substructure = P_Q_T <-- \x -> case run <-- lower x of
		ls :*: rs -> Store <--- ls :*: lift . (T_U . (:*: rs))

instance (Covariant (->) (->) t, Covariant (->) (->) u) => Substructure Right (t <:*:> u) where
	type Substance Right (t <:*:> u) = u
	substructure = P_Q_T <-- \x -> case run <-- lower x of
		ls :*: rs -> Store <--- rs :*: lift . (T_U . (ls :*:))
