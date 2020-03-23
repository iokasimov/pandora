module Pandora.Paradigm.Basis.Continuation (Continuation (..), cwcc, reset, shift) where

import Pandora.Core.Functor (type (:.), type (:=), type (::|:.))
import Pandora.Core.Morphism ((!), (%))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Pattern.Functor.Divariant (($))

newtype Continuation r t a = Continuation { continue :: (->) ::|:. a :. t := r }

instance Covariant t => Covariant (Continuation r t) where
	f <$> Continuation continuation = Continuation $ continuation . (. f)

instance Covariant t => Pointable (Continuation r t) where
	point x = Continuation ($ x)

instance Covariant t => Applicative (Continuation r t) where
	f <*> x = Continuation $ \h -> continue f $ \g -> continue x (h . g)

instance Covariant t => Bindable (Continuation r t) where
	x >>= f = Continuation $ \g -> continue x $ \y -> continue (f y) g

instance Monad t => Monad (Continuation r t) where

instance (forall u . Bindable u) => Liftable (Continuation r) where
	lift = Continuation . (>>=)

-- | Call with current continuation
cwcc :: ((a -> Continuation r t b) -> Continuation r t a) -> Continuation r t a
cwcc f = Continuation $ \g -> continue % g . f $ Continuation . (!) . g

-- | Delimit the continuation of any 'shift'
reset :: (forall u . Bindable u, Bindable t, Pointable t) => Continuation r t r -> Continuation s t r
reset = lift . continue % point

-- | Capture the continuation up to the nearest enclosing 'reset' and pass it
shift :: Pointable t => ((a -> t r) -> Continuation r t r) -> Continuation r t a
shift f = Continuation $ continue % point . f
