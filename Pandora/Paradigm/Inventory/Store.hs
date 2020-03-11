{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Store (Store (..), position, access, retrofit) where

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
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, unwrap))
import Pandora.Paradigm.Controlflow.Joint.Schematic (Schematic)
import Pandora.Paradigm.Controlflow.Joint.Schemes.TUV (TUV (TUV))
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
	unwrap (Store x) = x

type instance Schematic Comonad (Store p) u =
	TUV Covariant Covariant Covariant ((:*:) p) u ((->) p)

instance Comonadic (Store p) where
	flick (TC (TUV (p :*: f))) = ($ p) <$> f
	bring (TC (TUV (p :*: f))) = Store $ p :*: extract f

type Storable s x = Adaptable x (Store s)

instance Covariant u => Covariant (TUV Covariant Covariant Covariant ((:*:) p) u ((->) p)) where
	f <$> TUV (p :*: x) = TUV . (:*:) p $ f <$$> x

instance Extractable u => Extractable (TUV Covariant Covariant Covariant ((:*:) p) u ((->) p)) where
	extract (TUV (p :*: x)) = extract x p

instance Extendable u => Extendable (TUV Covariant Covariant Covariant ((:*:) p) u ((->) p)) where
	TUV (old :*: x) =>> f = TUV . (:*:) old $ x =>> (\x' new -> f . TUV . (:*:) new $ x')

position :: Storable s t => t a -> s
position = attached . unwrap @(Store _) . adapt

access :: Storable s t => s -> a <-| t
access p = extract % p . unwrap @(Store _) . adapt

retrofit :: (p -> p) -> Store p ~> Store p
retrofit g (Store (p :*: f)) = Store $ g p :*: f
