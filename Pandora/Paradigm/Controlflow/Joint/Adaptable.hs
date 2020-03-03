{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Controlflow.Joint.Adaptable (Adaptable (..)) where

import Pandora.Core.Transformation (type (~>))
import Pandora.Pattern.Category (identity, (.))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Pointable (Pointable)
import Pandora.Paradigm.Controlflow.Joint.Transformer (Transformer (Schema, lay, wrap), (:>))

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
	( Layable t (Schema u v)
	, Wrappable u v
	) => Adaptable u (t :> u :> v) where
	adapt = lay . wrap

instance
	( Layable t (Schema u v)
	, Layable u v
	) => Adaptable v (t :> u :> v) where
	adapt = lay . lay

instance
	( Layable t (Schema u (v :> w))
	, Layable u (Schema v w)
	, Wrappable v w
	) => Adaptable v (t :> u :> v :> w) where
	adapt = lay . lay . wrap

instance
	( Layable t (Schema u v)
	, Layable t (Schema u (v :> w))
	, Layable u (Schema v w)
	, Layable v w
	) => Adaptable w (t :> u :> v :> w) where
	adapt = lay . lay . lay

instance (Layable t (Schema u (v :> w :> x))
	, Layable u (Schema v (w :> x))
	, Layable v (Schema w x)
	, Layable w x
	) => Adaptable x (t :> u :> v :> w :> x) where
	adapt = lay . lay . lay . lay

instance (Layable t (Schema u (v :> w :> x))
	, Layable u (Schema v (w :> x))
	, Layable v (Schema w x)
	, Wrappable w x
	) => Adaptable w (t :> u :> v :> w :> x) where
	adapt = lay . lay . lay . wrap

instance
	( Layable t (Schema u (v :> w :> x :> y))
	, Layable u (Schema v (w :> x :> y))
	, Layable v (Schema w (x :> y))
	, Layable w (Schema x y)
	, Layable x y
	) => Adaptable y (t :> u :> v :> w :> x :> y) where
	adapt = lay . lay . lay . lay . lay

instance
	( Layable t (Schema u (v :> w :> x :> y))
	, Layable u (Schema v (w :> x :> y))
	, Layable v (Schema w (x :> y))
	, Layable w (Schema x y)
	, Wrappable x y
	) => Adaptable x (t :> u :> v :> w :> x :> y) where
	adapt = lay . lay . lay . lay . wrap

instance
	( Layable t (Schema u (v :> w :> x :> y :> z))
	, Layable u (Schema v (w :> x :> y :> z))
	, Layable v (Schema w (x :> y :> z))
	, Layable w (Schema x (y :> z))
	, Layable x (Schema y z)
	, Layable y z
	) => Adaptable z (t :> u :> v :> w :> x :> y :> z) where
	adapt = lay . lay . lay . lay . lay . lay

instance
	( Layable t (Schema u (v :> w :> x :> y :> z))
	, Layable u (Schema v (w :> x :> y :> z))
	, Layable v (Schema w (x :> y :> z))
	, Layable w (Schema x (y :> z))
	, Layable x (Schema y z)
	, Wrappable y z
	) => Adaptable y (t :> u :> v :> w :> x :> y :> z) where
	adapt = lay . lay . lay . lay . lay . wrap

instance
	( Layable t (Schema u (v :> w :> x :> y :> z :> f))
	, Layable u (Schema v (w :> x :> y :> z :> f))
	, Layable v (Schema w (x :> y :> z :> f))
	, Layable w (Schema x (y :> z :> f))
	, Layable x (Schema y (z :> f))
	, Layable y (Schema z f)
	, Layable z f
	) => Adaptable f (t :> u :> v :> w :> x :> y :> z :> f) where
	adapt = lay . lay . lay . lay . lay . lay . lay

instance
	( Layable t (Schema u (v :> w :> x :> y :> z :> f))
	, Layable u (Schema v (w :> x :> y :> z :> f))
	, Layable v (Schema w (x :> y :> z :> f))
	, Layable w (Schema x (y :> z :> f))
	, Layable x (Schema y (z :> f))
	, Layable y (Schema z f)
	, Wrappable z f
	) => Adaptable z (t :> u :> v :> w :> x :> y :> z :> f) where
	adapt = lay . lay . lay . lay . lay . lay . wrap

instance
	( Layable t (Schema u (v :> w :> x :> y :> z :> f :> h))
	, Layable u (Schema v (w :> x :> y :> z :> f :> h))
	, Layable v (Schema w (x :> y :> z :> f :> h))
	, Layable w (Schema x (y :> z :> f :> h))
	, Layable x (Schema y (z :> f :> h))
	, Layable y (Schema z (f :> h))
	, Layable z (Schema f h)
	, Layable f h
	) => Adaptable h (t :> u :> v :> w :> x :> y :> z :> f :> h) where
	adapt = lay . lay . lay . lay . lay . lay . lay . lay

instance
	( Layable t (Schema u (v :> w :> x :> y :> z :> f :> h))
	, Layable u (Schema v (w :> x :> y :> z :> f :> h))
	, Layable v (Schema w (x :> y :> z :> f :> h))
	, Layable w (Schema x (y :> z :> f :> h))
	, Layable x (Schema y (z :> f :> h))
	, Layable y (Schema z (f :> h))
	, Layable z (Schema f h)
	, Wrappable f h
	) => Adaptable f (t :> u :> v :> w :> x :> y :> z :> f :> h) where
	adapt = lay . lay . lay . lay . lay . lay . lay . wrap
