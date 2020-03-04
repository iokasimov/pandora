{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Inventory.Accumulator (Accumulator (..), Accumulated, gather) where

import Pandora.Core.Functor (Variant (Co))
import Pandora.Paradigm.Basis.Product (Product ((:*:)), type (:*:))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, unwrap))
import Pandora.Paradigm.Controlflow.Joint.Transformer (Transformer (lay, wrap), (:>) (T))
import Pandora.Paradigm.Controlflow.Joint.Schematic (Schematic)
import Pandora.Paradigm.Controlflow.Joint.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Controlflow.Joint.Schemes.UT (UT (UT))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))

newtype Accumulator e a = Accumulator (e :*: a)

instance Covariant (Accumulator e) where
	f <$> Accumulator x = Accumulator $ f <$> x

instance Semigroup e => Applicative (Accumulator e) where
	f <*> v = Accumulator $ k (unwrap f) (unwrap v) where
		k ~(e :*: g) ~(e' :*: w) = e + e' :*: g w

instance Monoid e => Pointable (Accumulator e) where
	point = Accumulator . (zero :*:)

instance Semigroup e => Bindable (Accumulator e) where
	Accumulator (e :*: x) >>= f = let (e' :*: b) = unwrap $ f x in
		Accumulator $ e + e':*: b

type instance Schematic Monad (Accumulator e) u = UT 'Co 'Co ((:*:) e) u

instance Interpreted (Accumulator e) where
	type Primary (Accumulator e) a = e :*: a
	unwrap (Accumulator x) = x

instance Monoid e => Transformer (Accumulator e) where
	lay x = T . UT $ (zero :*:) <$> x
	wrap = T . UT . point . unwrap

type Accumulated e t = Adaptable (Accumulator e) t

instance Covariant u => Covariant (UT 'Co 'Co ((:*:) e) u) where
	f <$> UT x = UT $ f <$$> x

instance (Semigroup e, Applicative u) => Applicative (UT 'Co 'Co ((:*:) e) u) where
	UT f <*> UT x = UT $ k <$> f <*> x where
		k ~(u :*: g) ~(v :*: y) = u + v :*: g y

instance (Pointable u, Monoid e) => Pointable (UT 'Co 'Co ((:*:) e) u) where
	point = UT . point . (zero :*:)

instance (Semigroup e, Pointable u, Bindable u) => Bindable (UT 'Co 'Co ((:*:) e) u) where
	UT x >>= f = UT $ x >>= \(acc :*: v) -> (\(acc' :*: y) -> (acc + acc' :*: y)) <$> unwrap (f v)

gather :: (Covariant t, Accumulated e t) => e -> t ()
gather x = adapt . Accumulator $ x :*: ()
