module Pandora.Paradigm.Inventory.Store (Store (..), position, access, retrofit) where

import Pandora.Core.Functor (type (:.), type (:=), type (<-|))
import Pandora.Core.Morphism ((%))
import Pandora.Core.Transformation (type (~>))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Paradigm.Basis.Product (Product ((:*:)), type (:*:))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, unwrap))
import Pandora.Paradigm.Controlflow.Joint.Schematic (Schematic)
import Pandora.Paradigm.Controlflow.Joint.Schemes.TUV (TUV (TUV))
import Pandora.Paradigm.Controlflow.Joint.Transformer.Comonadic (Comonadic (flick, bring), (:<) (TC))

newtype Store p a = Store ((:*:) p :. (->) p := a)

instance Covariant (Store p) where
	g <$> Store (p :*: f) = Store . (:*:) p $ (g .) f

instance Extractable (Store p) where
	extract (Store (p :*: f)) = f p

instance Extendable (Store p) where
	Store (old :*: f) =>> g = Store . (:*:) old
		$ \new -> g . Store $ new :*: f

instance Comonad (Store p) where

instance Interpreted (Store p) where
	type Primary (Store p) a = (:*:) p :. (->) p := a
	unwrap (Store x) = x

type instance Schematic Comonad (Store p) u = TUV Covariant Covariant Covariant ((:*:) p) u ((->) p)

instance Comonadic (Store p) where
	flick (TC (TUV (p :*: f))) = ($ p) <$> f
	bring (TC (TUV (p :*: f))) = Store $ p :*: extract f

position :: Store p a -> p
position (Store (p :*: _)) = p

access :: p -> a <-| Store p
access p = extract % p . unwrap

retrofit :: (p -> p) -> Store p ~> Store p
retrofit g (Store (p :*: f)) = Store $ g p :*: f
