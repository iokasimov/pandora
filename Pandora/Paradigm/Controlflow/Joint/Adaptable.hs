{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Controlflow.Joint.Adaptable (Adaptable (..)) where

import Pandora.Core.Transformation (type (~>))
import Pandora.Pattern.Category (identity, (.))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Pointable (Pointable)
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Paradigm.Controlflow.Joint.Transformer (Transformer (lay, wrap), (:>))
import Pandora.Paradigm.Controlflow.Joint.Schematic (Schematic)

class Adaptable eff schema where
	{-# MINIMAL adapt #-}
	adapt :: eff ~> schema

type Layable t u = (Transformer t, Covariant u)
type Wrappable t u = (Transformer t, Pointable u)

instance Adaptable t t where
	adapt = identity

instance Layable t u => Adaptable u (t :> u) where
	adapt = lay

instance Wrappable t u => Adaptable t (t :> u) where
	adapt = wrap

instance
	( Layable t (Schematic Monad u v)
	, Wrappable u v
	) => Adaptable u (t :> u :> v) where
	adapt = lay . wrap

instance
	( Layable t (Schematic Monad u v)
	, Layable u v
	) => Adaptable v (t :> u :> v) where
	adapt = lay . lay

instance
	( Layable t (Schematic Monad u (v :> w))
	, Layable u (Schematic Monad v w)
	, Wrappable v w
	) => Adaptable v (t :> u :> v :> w) where
	adapt = lay . lay . wrap

instance
	( Layable t (Schematic Monad u v)
	, Layable t (Schematic Monad u (v :> w))
	, Layable u (Schematic Monad v w)
	, Layable v w
	) => Adaptable w (t :> u :> v :> w) where
	adapt = lay . lay . lay

instance (Layable t (Schematic Monad u (v :> w :> x))
	, Layable u (Schematic Monad v (w :> x))
	, Layable v (Schematic Monad w x)
	, Layable w x
	) => Adaptable x (t :> u :> v :> w :> x) where
	adapt = lay . lay . lay . lay

instance (Layable t (Schematic Monad u (v :> w :> x))
	, Layable u (Schematic Monad v (w :> x))
	, Layable v (Schematic Monad w x)
	, Wrappable w x
	) => Adaptable w (t :> u :> v :> w :> x) where
	adapt = lay . lay . lay . wrap

instance
	( Layable t (Schematic Monad u (v :> w :> x :> y))
	, Layable u (Schematic Monad v (w :> x :> y))
	, Layable v (Schematic Monad w (x :> y))
	, Layable w (Schematic Monad x y)
	, Layable x y
	) => Adaptable y (t :> u :> v :> w :> x :> y) where
	adapt = lay . lay . lay . lay . lay

instance
	( Layable t (Schematic Monad u (v :> w :> x :> y))
	, Layable u (Schematic Monad v (w :> x :> y))
	, Layable v (Schematic Monad w (x :> y))
	, Layable w (Schematic Monad x y)
	, Wrappable x y
	) => Adaptable x (t :> u :> v :> w :> x :> y) where
	adapt = lay . lay . lay . lay . wrap

instance
	( Layable t (Schematic Monad u (v :> w :> x :> y :> z))
	, Layable u (Schematic Monad v (w :> x :> y :> z))
	, Layable v (Schematic Monad w (x :> y :> z))
	, Layable w (Schematic Monad x (y :> z))
	, Layable x (Schematic Monad y z)
	, Layable y z
	) => Adaptable z (t :> u :> v :> w :> x :> y :> z) where
	adapt = lay . lay . lay . lay . lay . lay

instance
	( Layable t (Schematic Monad u (v :> w :> x :> y :> z))
	, Layable u (Schematic Monad v (w :> x :> y :> z))
	, Layable v (Schematic Monad w (x :> y :> z))
	, Layable w (Schematic Monad x (y :> z))
	, Layable x (Schematic Monad y z)
	, Wrappable y z
	) => Adaptable y (t :> u :> v :> w :> x :> y :> z) where
	adapt = lay . lay . lay . lay . lay . wrap

instance
	( Layable t (Schematic Monad u (v :> w :> x :> y :> z :> f))
	, Layable u (Schematic Monad v (w :> x :> y :> z :> f))
	, Layable v (Schematic Monad w (x :> y :> z :> f))
	, Layable w (Schematic Monad x (y :> z :> f))
	, Layable x (Schematic Monad y (z :> f))
	, Layable y (Schematic Monad z f)
	, Layable z f
	) => Adaptable f (t :> u :> v :> w :> x :> y :> z :> f) where
	adapt = lay . lay . lay . lay . lay . lay . lay

instance
	( Layable t (Schematic Monad u (v :> w :> x :> y :> z :> f))
	, Layable u (Schematic Monad v (w :> x :> y :> z :> f))
	, Layable v (Schematic Monad w (x :> y :> z :> f))
	, Layable w (Schematic Monad x (y :> z :> f))
	, Layable x (Schematic Monad y (z :> f))
	, Layable y (Schematic Monad z f)
	, Wrappable z f
	) => Adaptable z (t :> u :> v :> w :> x :> y :> z :> f) where
	adapt = lay . lay . lay . lay . lay . lay . wrap

instance
	( Layable t (Schematic Monad u (v :> w :> x :> y :> z :> f :> h))
	, Layable u (Schematic Monad v (w :> x :> y :> z :> f :> h))
	, Layable v (Schematic Monad w (x :> y :> z :> f :> h))
	, Layable w (Schematic Monad x (y :> z :> f :> h))
	, Layable x (Schematic Monad y (z :> f :> h))
	, Layable y (Schematic Monad z (f :> h))
	, Layable z (Schematic Monad f h)
	, Layable f h
	) => Adaptable h (t :> u :> v :> w :> x :> y :> z :> f :> h) where
	adapt = lay . lay . lay . lay . lay . lay . lay . lay

instance
	( Layable t (Schematic Monad u (v :> w :> x :> y :> z :> f :> h))
	, Layable u (Schematic Monad v (w :> x :> y :> z :> f :> h))
	, Layable v (Schematic Monad w (x :> y :> z :> f :> h))
	, Layable w (Schematic Monad x (y :> z :> f :> h))
	, Layable x (Schematic Monad y (z :> f :> h))
	, Layable y (Schematic Monad z (f :> h))
	, Layable z (Schematic Monad f h)
	, Wrappable f h
	) => Adaptable f (t :> u :> v :> w :> x :> y :> z :> f :> h) where
	adapt = lay . lay . lay . lay . lay . lay . lay . wrap
