{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Store (Store (..), Storable, position, access, retrofit) where

import Pandora.Core.Functor (type (:.), type (:=), type (<-|), type (~>))
import Pandora.Core.Morphism ((%))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Paradigm.Basis.Product (Product ((:*:)), type (:*:), attached)
import Pandora.Paradigm.Controlflow.Joint.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))
import Pandora.Paradigm.Controlflow.Joint.Schematic (Schematic)
import Pandora.Paradigm.Controlflow.Joint.Schemes.TUT (TUT (TUT))
import Pandora.Paradigm.Controlflow.Joint.Transformer.Comonadic (Comonadic (flick, bring), (:<) (TC))

newtype Store p a = Store ((:*:) p :. (->) p := a)

instance Covariant (Store p) where
	f <$> Store (p :*: x) = Store . (:*:) p $ f <$> x

instance Extractable (Store p) where
	extract (Store (p :*: f)) = f p

instance Extendable (Store p) where
	Store (old :*: x) =>> f = Store . (:*:) old
		$ \new -> f <$> Store $ new :*: x

instance Comonad (Store p) where

instance Interpreted (Store p) where
	type Primary (Store p) a = (:*:) p :. (->) p := a
	run (Store x) = x

type instance Schematic Comonad (Store p) u =
	TUT Covariant Covariant Covariant ((:*:) p) u ((->) p)

instance Comonadic (Store p) where
	flick (TC (TUT (p :*: f))) = ($ p) <$> f
	bring (TC (TUT (p :*: f))) = Store $ p :*: extract f

type Storable s x = Adaptable x (Store s)

instance Covariant u => Covariant (TUT Covariant Covariant Covariant ((:*:) p) u ((->) p)) where
	f <$> TUT (p :*: x) = TUT . (:*:) p $ f <$$> x

instance Extractable u => Extractable (TUT Covariant Covariant Covariant ((:*:) p) u ((->) p)) where
	extract (TUT (p :*: x)) = extract x p

instance Extendable u => Extendable (TUT Covariant Covariant Covariant ((:*:) p) u ((->) p)) where
	TUT (old :*: x) =>> f = TUT . (:*:) old $ x =>> (\x' new -> f . TUT . (:*:) new $ x')

position :: Storable s t => t a -> s
position = attached . run @(Store _) . adapt

access :: Storable s t => s -> a <-| t
access p = extract % p . run @(Store _) . adapt

retrofit :: (p -> p) -> Store p ~> Store p
retrofit g (Store (p :*: f)) = Store $ g p :*: f
