{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Optics where

import Pandora.Core.Impliable (Impliable (Arguments, imply))
import Pandora.Pattern.Category (Category (identity, (.), ($), (#)))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Invariant (Invariant ((<$<)))
import Pandora.Pattern.Functor.Divariant ((>->))
import Pandora.Pattern.Functor.Representable (Representable (Representation, (<#>), tabulate))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (run))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Function ((!))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Transformer.Flip (Flip (Flip))
import Pandora.Paradigm.Primary.Object.Boolean ((?))
import Pandora.Paradigm.Inventory.Store (Store (Store), position, look, retrofit)
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))
import Pandora.Paradigm.Structure.Ability.Monotonic (resolve)

infixr 0 :-.
infixl 2 #=@

type Lens = P_Q_T (->) Store

type (:-.) source target = Lens Identity source target

type family Convex lens where
	Convex Lens = Lens Identity

instance Category (Lens Identity) where
	identity = P_Q_T $ \source -> Store $ Identity source :*: identity . extract
	P_Q_T to . P_Q_T from = P_Q_T $ \source -> source <$ (to . extract @Identity . position $ from source)

instance Invariant (Flip (Lens available) tgt) where
	f <$< g = \(Flip (P_Q_T lens)) -> Flip . P_Q_T $ g >-> (f <$>) $ lens

instance Impliable (P_Q_T (->) Store Identity source target) where
	type Arguments (P_Q_T (->) Store Identity source target) =
		(source -> target) -> (source -> target -> source) -> P_Q_T (->) Store Identity source target
	imply getter setter = P_Q_T $ \source -> Store $ Identity # getter source :*: setter source . extract

type family Obscure lens where
	Obscure Lens = Lens Maybe

instance Category (Lens Maybe) where
	identity = P_Q_T $ \source -> Store $ Just source :*: resolve identity source
	P_Q_T to . P_Q_T from = P_Q_T $ \source -> case position (from source) of
		Nothing -> Store $ Nothing :*: (source !)
		Just between -> source <$ to between

-- Lens as natural transformation
type (#=@) source target available = forall a . Lens available (source a) (target a)

-- | Get focused target value
view :: Lens available source target -> source -> available target
view lens = position @_ @(Store _) . run lens

-- Replace focused target value with new value
set :: Lens available source target -> available target -> source -> source
set lens new = look new . run lens

-- | Modify focused target value
over :: Covariant available => Lens available source target -> (available target -> available target) -> source -> source
over lens f = extract . retrofit f . run lens

-- | Representable based lens
represent :: (Representable t, Setoid (Representation t)) => Representation t -> Convex Lens (t a) a
represent r = P_Q_T $ \x -> Store $ Identity (r <#> x) :*: \new -> tabulate (\r' -> r' == r ? extract new $ r' <#> x)
