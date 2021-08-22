{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Store where

import Pandora.Core (type (:.), type (:=), type (<:=), type (~>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (identity, ($))
import Pandora.Pattern.Functor.Covariant (Covariant ((-<$>-)), (-<$$>-))
import Pandora.Pattern.Functor.Semimonoidal (Semimonoidal (multiply))
import Pandora.Pattern.Functor.Monoidal (Monoidal (unit))
import Pandora.Pattern.Functor.Invariant (Invariant ((<$<)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Bivariant ((<->))
import Pandora.Pattern.Functor.Divariant ((>->))
import Pandora.Pattern.Functor.Adjoint ((-|))
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (<--), (%), (!.), (-.#..-))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)), attached)
import Pandora.Paradigm.Primary.Algebraic (extract)
import Pandora.Paradigm.Primary.Transformer.Flip (Flip (Flip))
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite, (||=)), Schematic)
import Pandora.Paradigm.Controlflow.Effect.Transformer.Comonadic (Comonadic (bring), (:<) (TC))
import Pandora.Paradigm.Schemes.TUT (TUT (TUT), type (<:<.>:>))

-- | Context based computation on value
newtype Store s a = Store ((:*:) s :. (->) s := a)

instance Covariant (->) (->) (Store s) where
	f -<$>- Store x = Store $ f -<$$>- x

instance Semimonoidal (<--) (:*:) (:*:) (Store s) where
	multiply = Flip $ \(Store (s :*: f)) -> 
		let (x :*: y) = f s in
		Store (s :*: (x !.)) :*: Store (s :*: (y !.))

instance Monoidal (Store s) (<--) (->) (:*:) (:*:) where
	unit _ = Flip $ \(Store (s :*: f)) -> (\_ -> f s)

instance Extendable (Store s) (->) where
	f <<= Store x = Store $ f -<$$>- (Store -.#..- (identity @(->) -|) -<$>- x)

instance Comonad (Store s) (->) where

instance Invariant (Flip Store r) where
	f <$< g = \(Flip x) -> Flip $ (<->) @_ @_ @(->) f (g >-> identity @(->)) ||= x

instance Interpreted (Store s) where
	type Primary (Store s) a = (:*:) s :. (->) s := a
	run ~(Store x) = x
	unite = Store

type instance Schematic Comonad (Store s) = (:*:) s <:<.>:> (->) s

instance Comonadic (Store s) where
	bring (TC (TUT (s :*: f))) = Store $ s :*: extract f

type Storable s x = Adaptable x (Store s)

-- | Get current index
position :: Storable s t => t a -> s
position = attached . run @(Store _) . adapt

-- | Given an index return value
look :: Storable s t => s -> a <:= t
look s = (extract % s) . run @(Store _) . adapt

-- | Change index with function
retrofit :: (s -> s) -> Store s ~> Store s
retrofit g (Store (s :*: f)) = Store $ g s :*: f
