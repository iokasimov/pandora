{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Optics where

import Pandora.Core.Impliable (Impliable (Arguments, imply))
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category (Category (identity, ($), (#)))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Invariant (Invariant ((<$<)))
import Pandora.Pattern.Functor.Divariant ((>->))
import Pandora.Pattern.Functor.Representable (Representable (Representation, (<#>), tabulate))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (run))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Primary.Algebraic.Exponential ((!.), (%))
import Pandora.Paradigm.Primary.Algebraic (($>-), extract)
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Paradigm.Primary.Object.Boolean ((?))
import Pandora.Paradigm.Inventory.Store (Store (Store), position, look, retrofit)
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))
import Pandora.Paradigm.Structure.Ability.Monotonic (resolve)

infixl 2 #=@

type Lens = P_Q_T (->) Store

instance Invariant (Flip (Lens available) tgt) where
	f <$< g = \(Flip (P_Q_T lens)) -> Flip . P_Q_T $ g >-> (f <-|-) $ lens

type family Convex lens where
	Convex Lens = Lens Identity

instance Semigroupoid (Lens Identity) where
	(.) :: Convex Lens between target -> Convex Lens source between -> Convex Lens source target
	P_Q_T to . P_Q_T from = P_Q_T $ \source -> (to . extract @Identity . position $ from source) $>- source

instance Category (Lens Identity) where
	identity :: Convex Lens source source
	identity = imply @(Convex Lens _ _) identity ((%) (!.))

instance Impliable (P_Q_T (->) Store Identity source target) where
	type Arguments (P_Q_T (->) Store Identity source target) =
		(source -> target) -> (source -> target -> source) -> Lens Identity source target
	imply getter setter = P_Q_T $ \source -> Store $ Identity # getter source :*: setter source . extract

type family Obscure lens where
	Obscure Lens = Lens Maybe

instance Impliable (P_Q_T (->) Store Maybe source target) where
	type Arguments (P_Q_T (->) Store Maybe source target) =
		(source -> Maybe target) -> (source -> Maybe target -> source) -> Lens Maybe source target
	imply getter setter = P_Q_T $ \source -> Store $ getter source :*: setter source

instance Semigroupoid (Lens Maybe) where
	(.) :: Obscure Lens between target -> Obscure Lens source between -> Obscure Lens source target
	P_Q_T to . P_Q_T from = P_Q_T $ \source -> case position # from source of
		Nothing -> Store $ Nothing :*: (source !.)
		Just between -> to between $>- source

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
represent r = imply @(Convex Lens (t a) a) (r <#>) (\source target -> tabulate $ \r' -> r' == r ? target $ r' <#> source)
