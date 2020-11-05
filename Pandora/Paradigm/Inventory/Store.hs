{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Store where

import Pandora.Core (type (:.), type (:=), type (<-|), type (~>), (%))
import Pandora.Pattern ((.|..))
import Pandora.Pattern.Category (identity, (.), ($))
import Pandora.Pattern.Functor (Covariant ((<$>), (<$$>), (<$$$>)), Extractable (extract), Extendable ((=>>), (<<=$)), Comonad, (-|), (|-))
import Pandora.Paradigm.Primary.Functor (Product ((:*:)), type (:*:), attached)
import Pandora.Paradigm.Controlflow (Adaptable (adapt), Interpreted (Primary, run), Schematic, Comonadic (bring), (:<) (TC))
import Pandora.Paradigm.Schemes.TUT (TUT (TUT), type (<:<.>:>))

newtype Store p a = Store ((:*:) p :. (->) p := a)

instance Covariant (Store p) where
	f <$> Store x = Store $ f <$$> x

instance Extractable (Store p) where
	extract = (|- ($)) . run

instance Extendable (Store p) where
	Store x =>> f = Store $ f <$$> (Store .|.. (-| identity)) <$> x

instance Comonad (Store p) where

instance Interpreted (Store p) where
	type Primary (Store p) a = (:*:) p :. (->) p := a
	run (Store x) = x

type instance Schematic Comonad (Store p) = (:*:) p <:<.>:> (->) p

instance Comonadic (Store p) where
	bring (TC (TUT (p :*: f))) = Store $ p :*: extract f

type Storable s x = Adaptable x (Store s)

instance Covariant u => Covariant ((:*:) p <:<.>:> (->) p := u) where
	f <$> TUT x = TUT $ f <$$$> x

instance Extractable u => Extractable ((:*:) p <:<.>:> (->) p := u) where
	extract = (|- extract) . run

instance Extendable u => Extendable ((:*:) p <:<.>:> (->) p := u) where
	TUT x =>> f = TUT $ x <<=$ (\x' -> f . TUT . (x' -| identity))

position :: Storable s t => t a -> s
position = attached . run @(Store _) . adapt

access :: Storable s t => s -> a <-| t
access p = extract % p . run @(Store _) . adapt

retrofit :: (p -> p) -> Store p ~> Store p
retrofit g (Store (p :*: f)) = Store $ g p :*: f
