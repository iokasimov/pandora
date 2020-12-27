{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Store where

import Pandora.Core (type (:.), type (:=), type (<-|), type (~>), (%))
import Pandora.Pattern ((.|..))
import Pandora.Pattern.Category (identity, (.), ($))
import Pandora.Pattern.Functor (Covariant ((<$>), (<$$>)), Extractable (extract), Extendable ((=>>), (<<=$)), Comonad, (-|), (|-))
import Pandora.Paradigm.Primary.Functor (Product ((:*:)), type (:*:), attached)
import Pandora.Paradigm.Controlflow (Adaptable (adapt), Interpreted (Primary, run, unite), Schematic, Comonadic (bring), (:<) (TC))
import Pandora.Paradigm.Schemes.TUT (TUT (TUT), type (<:<.>:>))

-- | Context based computation on value
newtype Store s a = Store ((:*:) s :. (->) s := a)

instance Covariant (Store s) where
	f <$> Store x = Store $ f <$$> x

instance Extractable (Store s) where
	extract = (|- ($)) . run

instance Extendable (Store s) where
	Store x =>> f = Store $ f <$$> (Store .|.. (-| identity)) <$> x

instance Comonad (Store s) where

instance Interpreted (Store s) where
	type Primary (Store s) a = (:*:) s :. (->) s := a
	run ~(Store x) = x
	unite = Store

type instance Schematic Comonad (Store s) = (:*:) s <:<.>:> (->) s

instance Comonadic (Store s) where
	bring (TC (TUT (s :*: f))) = Store $ s :*: extract f

type Storable s x = Adaptable x (Store s)

instance {-# OVERLAPS #-} Extendable u => Extendable ((:*:) s <:<.>:> (->) s := u) where
	TUT x =>> f = TUT $ x <<=$ (\x' -> f . TUT . (x' -| identity))

-- | Get current index
position :: Storable s t => t a -> s
position = attached . run @(Store _) . adapt

-- | Given an index return value
access :: Storable s t => s -> a <-| t
access s = extract % s . run @(Store _) . adapt

-- | Change index with function
retrofit :: (s -> s) -> Store s ~> Store s
retrofit g (Store (s :*: f)) = Store $ g s :*: f
