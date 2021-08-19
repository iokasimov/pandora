{-# LANGUAGE UndecidableInstances #-}

module Pandora.Paradigm.Controlflow.Effect.Adaptable where

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (identity)
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Monoidal (Monoidal)
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Transformer (Liftable (lift), Lowerable (lower), Hoistable (hoist))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:))
import Pandora.Paradigm.Primary.Algebraic (Extractable_)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Schematic)
import Pandora.Paradigm.Controlflow.Effect.Transformer (Monadic, Comonadic, wrap, bring, (:>), (:<))

class Adaptable t u where
	{-# MINIMAL adapt #-}
	adapt :: t ~> u

type Lifting t u = (Monadic t, Liftable (Schematic Monad t), Covariant u (->) (->))
type Lowering t u = (Comonadic t, Lowerable (Schematic Comonad t), Covariant u (->) (->))
type Wrappable t u = (Monadic t, Monoidal u (->) (->) (:*:) (:*:))
type Bringable t u = (Comonadic t, Extractable_ u)

instance Adaptable t t where
	adapt = identity

instance Lifting t u => Adaptable u (t :> u) where
	adapt = lift

instance Wrappable t u => Adaptable t (t :> u) where
	adapt = wrap

instance Lowering t u => Adaptable (t :< u) u where
	adapt = lower

instance Bringable t u => Adaptable (t :< u) t where
	adapt = bring

instance
	( Liftable (Schematic Monad t)
	, Covariant (Schematic Monad u v) (->) (->)
	, Wrappable u v
	) => Adaptable u (t :> u :> v) where
	adapt = lift . wrap

instance
	( Lifting t (Schematic Monad u v)
	, Lifting u v
	) => Adaptable v (t :> u :> v) where
	adapt = lift . lift

instance
	( Lowering t (Schematic Comonad u v)
	, Bringable u v
	) => Adaptable (t :< u :< v) u where
	adapt = bring . lower

instance
	( Lowering t (Schematic Comonad u v)
	, Lowering u v
	) => Adaptable (t :< u :< v) v where
	adapt = lower . lower

instance
	( Liftable (Schematic Monad t)
	, Lifting t (Schematic Monad u (v :> w))
	, Lifting u (Schematic Monad v w)
	, Wrappable v w
	) => Adaptable v (t :> u :> v :> w) where
	adapt = lift . lift . wrap

instance
	( Lifting t (Schematic Monad u v)
	, Lifting t (Schematic Monad u (v :> w))
	, Lifting u (Schematic Monad v w)
	, Lifting v w
	) => Adaptable w (t :> u :> v :> w) where
	adapt = lift . lift . lift

instance
	( Lowering t (Schematic Comonad u (v :< w))
	, Lowering u (Schematic Comonad v w)
	, Bringable v w
	) => Adaptable (t :< u :< v :< w) v where
	adapt = bring . lower . lower

instance
	( Lowering t (Schematic Comonad u v)
	, Lowering t (Schematic Comonad u (v :< w))
	, Lowering u (Schematic Comonad v w)
	, Lowering v w
	) => Adaptable (t :< u :< v :< w) w where
	adapt = lower . lower . lower

instance
	( Lifting t (Schematic Monad u (v :> w :> x))
	, Lifting u (Schematic Monad v (w :> x))
	, Lifting v (Schematic Monad w x)
	, Lifting w x
	) => Adaptable x (t :> u :> v :> w :> x) where
	adapt = lift . lift . lift . lift

instance
	( Lifting t (Schematic Monad u (v :> w :> x))
	, Lifting u (Schematic Monad v (w :> x))
	, Lifting v (Schematic Monad w x)
	, Wrappable w x
	) => Adaptable w (t :> u :> v :> w :> x) where
	adapt = lift . lift . lift . wrap

instance
	( Lowering t (Schematic Comonad u (v :< w :< x))
	, Lowering u (Schematic Comonad v (w :< x))
	, Lowering v (Schematic Comonad w x)
	, Lowering w x
	) => Adaptable (t :< u :< v :< w :< x) x where
	adapt = lower . lower . lower . lower

instance
	( Lowering t (Schematic Comonad u (v :< w :< x))
	, Lowering u (Schematic Comonad v (w :< x))
	, Lowering v (Schematic Comonad w x)
	, Bringable w x
	) => Adaptable (t :< u :< v :< w :< x) w where
	adapt = bring . lower . lower . lower

instance
	( Lifting t (Schematic Monad u (v :> w :> x :> y))
	, Lifting u (Schematic Monad v (w :> x :> y))
	, Lifting v (Schematic Monad w (x :> y))
	, Lifting w (Schematic Monad x y)
	, Lifting x y
	) => Adaptable y (t :> u :> v :> w :> x :> y) where
	adapt = lift . lift . lift . lift . lift

instance
	( Lifting t (Schematic Monad u (v :> w :> x :> y))
	, Lifting u (Schematic Monad v (w :> x :> y))
	, Lifting v (Schematic Monad w (x :> y))
	, Lifting w (Schematic Monad x y)
	, Wrappable x y
	) => Adaptable x (t :> u :> v :> w :> x :> y) where
	adapt = lift . lift . lift . lift . wrap

instance
	( Lowering t (Schematic Comonad u (v :< w :< x :< y))
	, Lowering u (Schematic Comonad v (w :< x :< y))
	, Lowering v (Schematic Comonad w (x :< y))
	, Lowering w (Schematic Comonad x y)
	, Lowering x y
	) => Adaptable (t :< u :< v :< w :< x :< y) y where
	adapt = lower . lower . lower . lower . lower

instance
	( Lowering t (Schematic Comonad u (v :< w :< x :< y))
	, Lowering u (Schematic Comonad v (w :< x :< y))
	, Lowering v (Schematic Comonad w (x :< y))
	, Lowering w (Schematic Comonad x y)
	, Bringable x y
	) => Adaptable (t :< u :< v :< w :< x :< y) x where
	adapt = bring . lower . lower . lower . lower

instance
	( Lifting t (Schematic Monad u (v :> w :> x :> y :> z))
	, Lifting u (Schematic Monad v (w :> x :> y :> z))
	, Lifting v (Schematic Monad w (x :> y :> z))
	, Lifting w (Schematic Monad x (y :> z))
	, Lifting x (Schematic Monad y z)
	, Lifting y z
	) => Adaptable z (t :> u :> v :> w :> x :> y :> z) where
	adapt = lift . lift . lift . lift . lift . lift

instance
	( Lifting t (Schematic Monad u (v :> w :> x :> y :> z))
	, Lifting u (Schematic Monad v (w :> x :> y :> z))
	, Lifting v (Schematic Monad w (x :> y :> z))
	, Lifting w (Schematic Monad x (y :> z))
	, Lifting x (Schematic Monad y z)
	, Wrappable y z
	) => Adaptable y (t :> u :> v :> w :> x :> y :> z) where
	adapt = lift . lift . lift . lift . lift . wrap

instance
	( Lowering t (Schematic Comonad u (v :< w :< x :< y :< z))
	, Lowering u (Schematic Comonad v (w :< x :< y :< z))
	, Lowering v (Schematic Comonad w (x :< y :< z))
	, Lowering w (Schematic Comonad x (y :< z))
	, Lowering x (Schematic Comonad y z)
	, Lowering y z
	) => Adaptable (t :< u :< v :< w :< x :< y :< z) z where
	adapt = lower . lower . lower . lower . lower . lower

instance
	( Lowering t (Schematic Comonad u (v :< w :< x :< y :< z))
	, Lowering u (Schematic Comonad v (w :< x :< y :< z))
	, Lowering v (Schematic Comonad w (x :< y :< z))
	, Lowering w (Schematic Comonad x (y :< z))
	, Lowering x (Schematic Comonad y z)
	, Bringable y z
	) => Adaptable (t :< u :< v :< w :< x :< y :< z) y where
	adapt = bring . lower . lower . lower . lower . lower

instance
	( Lifting t (Schematic Monad u (v :> w :> x :> y :> z :> f))
	, Lifting u (Schematic Monad v (w :> x :> y :> z :> f))
	, Lifting v (Schematic Monad w (x :> y :> z :> f))
	, Lifting w (Schematic Monad x (y :> z :> f))
	, Lifting x (Schematic Monad y (z :> f))
	, Lifting y (Schematic Monad z f)
	, Lifting z f
	) => Adaptable f (t :> u :> v :> w :> x :> y :> z :> f) where
	adapt = lift . lift . lift . lift . lift . lift . lift

instance
	( Lifting t (Schematic Monad u (v :> w :> x :> y :> z :> f))
	, Lifting u (Schematic Monad v (w :> x :> y :> z :> f))
	, Lifting v (Schematic Monad w (x :> y :> z :> f))
	, Lifting w (Schematic Monad x (y :> z :> f))
	, Lifting x (Schematic Monad y (z :> f))
	, Lifting y (Schematic Monad z f)
	, Wrappable z f
	) => Adaptable z (t :> u :> v :> w :> x :> y :> z :> f) where
	adapt = lift . lift . lift . lift . lift . lift . wrap

instance
	( Lowering t (Schematic Comonad u (v :< w :< x :< y :< z :< f))
	, Lowering u (Schematic Comonad v (w :< x :< y :< z :< f))
	, Lowering v (Schematic Comonad w (x :< y :< z :< f))
	, Lowering w (Schematic Comonad x (y :< z :< f))
	, Lowering x (Schematic Comonad y (z :< f))
	, Lowering y (Schematic Comonad z f)
	, Lowering z f
	) => Adaptable (t :< u :< v :< w :< x :< y :< z :< f) f where
	adapt = lower . lower . lower . lower . lower . lower . lower

instance
	( Lowering t (Schematic Comonad u (v :< w :< x :< y :< z :< f))
	, Lowering u (Schematic Comonad v (w :< x :< y :< z :< f))
	, Lowering v (Schematic Comonad w (x :< y :< z :< f))
	, Lowering w (Schematic Comonad x (y :< z :< f))
	, Lowering x (Schematic Comonad y (z :< f))
	, Lowering y (Schematic Comonad z f)
	, Bringable z f
	) => Adaptable (t :< u :< v :< w :< x :< y :< z :< f) z where
	adapt = bring . lower . lower . lower . lower . lower . lower

instance
	( Lifting t (Schematic Monad u (v :> w :> x :> y :> z :> f :> h))
	, Lifting u (Schematic Monad v (w :> x :> y :> z :> f :> h))
	, Lifting v (Schematic Monad w (x :> y :> z :> f :> h))
	, Lifting w (Schematic Monad x (y :> z :> f :> h))
	, Lifting x (Schematic Monad y (z :> f :> h))
	, Lifting y (Schematic Monad z (f :> h))
	, Lifting z (Schematic Monad f h)
	, Lifting f h
	) => Adaptable h (t :> u :> v :> w :> x :> y :> z :> f :> h) where
	adapt = lift . lift . lift . lift . lift . lift . lift . lift

instance
	( Lifting t (Schematic Monad u (v :> w :> x :> y :> z :> f :> h))
	, Lifting u (Schematic Monad v (w :> x :> y :> z :> f :> h))
	, Lifting v (Schematic Monad w (x :> y :> z :> f :> h))
	, Lifting w (Schematic Monad x (y :> z :> f :> h))
	, Lifting x (Schematic Monad y (z :> f :> h))
	, Lifting y (Schematic Monad z (f :> h))
	, Lifting z (Schematic Monad f h)
	, Wrappable f h
	) => Adaptable f (t :> u :> v :> w :> x :> y :> z :> f :> h) where
	adapt = lift . lift . lift . lift . lift . lift . lift . wrap

instance
	( Lowering t (Schematic Comonad u (v :< w :< x :< y :< z :< f :< h))
	, Lowering u (Schematic Comonad v (w :< x :< y :< z :< f :< h))
	, Lowering v (Schematic Comonad w (x :< y :< z :< f :< h))
	, Lowering w (Schematic Comonad x (y :< z :< f :< h))
	, Lowering x (Schematic Comonad y (z :< f :< h))
	, Lowering y (Schematic Comonad z (f :< h))
	, Lowering z (Schematic Comonad f h)
	, Lowering f h
	) => Adaptable (t :< u :< v :< w :< x :< y :< z :< f :< h) h where
	adapt = lower . lower . lower . lower . lower . lower . lower . lower

instance
	( Lowering t (Schematic Comonad u (v :< w :< x :< y :< z :< f :< h))
	, Lowering u (Schematic Comonad v (w :< x :< y :< z :< f :< h))
	, Lowering v (Schematic Comonad w (x :< y :< z :< f :< h))
	, Lowering w (Schematic Comonad x (y :< z :< f :< h))
	, Lowering x (Schematic Comonad y (z :< f :< h))
	, Lowering y (Schematic Comonad z (f :< h))
	, Lowering z (Schematic Comonad f h)
	, Bringable f h
	) => Adaptable (t :< u :< v :< w :< x :< y :< z :< f :< h) f where
	adapt = bring . lower . lower . lower . lower . lower . lower . lower

instance (Covariant u (->) (->), Hoistable ((:>) t), Adaptable u u') => Adaptable (t :> u) (t :> u') where
	adapt = hoist adapt

instance
	( Covariant v (->) (->)
	, Covariant (Schematic Monad u v) (->) (->)
	, Hoistable ((:>) (t :> u))
	, Hoistable (Schematic Monad t)
	, Hoistable (Schematic Monad u)
	, Adaptable v v'
	) => Adaptable (t :> u :> v) (t :> u :> v') where
	adapt = hoist (hoist adapt)

instance
	( Covariant u (->) (->)
	, Covariant v (->) (->)
	, Covariant w (->) (->)
	, Covariant (Schematic Monad u v) (->) (->)
	, Covariant (Schematic Monad u (v :> w)) (->) (->)
	, Covariant (Schematic Monad v w) (->) (->)
	, Hoistable ((:>) (t :> u :> v))
	, Hoistable (Schematic Monad t)
	, Hoistable (Schematic Monad u)
	, Hoistable (Schematic Monad v)
	, Adaptable w w'
	) => Adaptable (t :> u :> v :> w) (t :> u :> v :> w') where
	adapt = hoist (hoist (hoist adapt))

instance
	( Covariant x (->) (->)
	, Covariant (Schematic Monad u (v :> (w :> x))) (->) (->)
	, Covariant (Schematic Monad v (w :> x)) (->) (->)
	, Covariant (Schematic Monad w x) (->) (->)
	, Hoistable ((:>) (t :> u :> v))
	, Hoistable (Schematic Monad t)
	, Hoistable (Schematic Monad u)
	, Hoistable (Schematic Monad v)
	, Hoistable (Schematic Monad w)
	, Adaptable x x'
	) => Adaptable (t :> u :> v :> w :> x) (t :> u :> v :> w :> x') where
	adapt = hoist (hoist (hoist (hoist adapt)))

instance
	( Covariant y (->) (->)
	, Covariant (Schematic Monad u (v :> (w :> (x :> y)))) (->) (->)
	, Covariant (Schematic Monad v (w :> (x :> y))) (->) (->)
	, Covariant (Schematic Monad w (x :> y)) (->) (->)
	, Covariant (Schematic Monad x y) (->) (->)
	, Hoistable ((:>) (t :> u :> v :> w))
	, Hoistable (Schematic Monad t)
	, Hoistable (Schematic Monad u)
	, Hoistable (Schematic Monad v)
	, Hoistable (Schematic Monad w)
	, Hoistable (Schematic Monad x)
	, Adaptable y y'
	) => Adaptable (t :> u :> v :> w :> x :> y) (t :> u :> v :> w :> x :> y') where
	adapt = hoist (hoist (hoist (hoist (hoist adapt))))

instance
	( Covariant z (->) (->)
	, Covariant (Schematic Monad u (v :> (w :> (x :> (y :> z))))) (->) (->)
	, Covariant (Schematic Monad v (w :> (x :> (y :> z)))) (->) (->)
	, Covariant (Schematic Monad w (x :> (y :> z))) (->) (->)
	, Covariant (Schematic Monad x (y :> z)) (->) (->)
	, Covariant (Schematic Monad y z) (->) (->)
	, Hoistable ((:>) (t :> u :> v :> w))
	, Hoistable (Schematic Monad t)
	, Hoistable (Schematic Monad u)
	, Hoistable (Schematic Monad v)
	, Hoistable (Schematic Monad w)
	, Hoistable (Schematic Monad x)
	, Hoistable (Schematic Monad y)
	, Adaptable z z'
	) => Adaptable (t :> u :> v :> w :> x :> y :> z)
		(t :> u :> v :> w :> x :> y :> z') where
	adapt = hoist (hoist (hoist (hoist (hoist adapt))))

instance
	( Covariant f (->) (->)
	, Covariant (Schematic Monad u (v :> (w :> (x :> (y :> (z :> f)))))) (->) (->)
	, Covariant (Schematic Monad v (w :> (x :> (y :> (z :> f))))) (->) (->)
	, Covariant (Schematic Monad w (x :> (y :> (z :> f)))) (->) (->)
	, Covariant (Schematic Monad x (y :> (z :> f))) (->) (->)
	, Covariant (Schematic Monad y (z :> f)) (->) (->)
	, Covariant (Schematic Monad z f) (->) (->)
	, Hoistable ((:>) (t :> u :> v :> w))
	, Hoistable (Schematic Monad t)
	, Hoistable (Schematic Monad u)
	, Hoistable (Schematic Monad v)
	, Hoistable (Schematic Monad w)
	, Hoistable (Schematic Monad x)
	, Hoistable (Schematic Monad y)
	, Hoistable (Schematic Monad z)
	, Adaptable f f'
	) => Adaptable (t :> u :> v :> w :> x :> y :> z :> f)
		(t :> u :> v :> w :> x :> y :> z :> f') where
	adapt = hoist (hoist (hoist (hoist (hoist (hoist adapt)))))

instance
	( Covariant h (->) (->)
	, Covariant (Schematic Monad u (v :> (w :> (x :> (y :> (z :> (f :> h))))))) (->) (->)
	, Covariant (Schematic Monad v (w :> (x :> (y :> (z :> (f :> h)))))) (->) (->)
	, Covariant (Schematic Monad w (x :> (y :> (z :> (f :> h))))) (->) (->)
	, Covariant (Schematic Monad x (y :> (z :> (f :> h)))) (->) (->)
	, Covariant (Schematic Monad y (z :> (f :> h))) (->) (->)
	, Covariant (Schematic Monad z (f :> h)) (->) (->)
	, Covariant (Schematic Monad f h) (->) (->)
	, Hoistable ((:>) (t :> u :> v :> w))
	, Hoistable (Schematic Monad t)
	, Hoistable (Schematic Monad u)
	, Hoistable (Schematic Monad v)
	, Hoistable (Schematic Monad w)
	, Hoistable (Schematic Monad x)
	, Hoistable (Schematic Monad y)
	, Hoistable (Schematic Monad z)
	, Hoistable (Schematic Monad f)
	, Adaptable h h'
	) => Adaptable (t :> u :> v :> w :> x :> y :> z :> f :> h)
		(t :> u :> v :> w :> x :> y :> z :> f :> h') where
	adapt = hoist (hoist (hoist (hoist (hoist (hoist (hoist adapt))))))
