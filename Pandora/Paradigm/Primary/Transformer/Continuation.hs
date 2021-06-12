module Pandora.Paradigm.Primary.Transformer.Continuation where

import Pandora.Core.Functor (type (:.), type (:=), type (::|:.))
import Pandora.Pattern.Category ((.), ($), (#))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Traversable (Traversable)
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))
import Pandora.Paradigm.Primary.Functor.Function ((!.), (%))

newtype Continuation r t a = Continuation ((->) ::|:. a :. t := r)

instance Interpreted (Continuation r t) where
	type Primary (Continuation r t) a = (->) ::|:. a :. t := r
	run ~(Continuation x) = x
	unite = Continuation

instance Covariant t => Covariant (Continuation r t) where
	f <$> Continuation continuation = Continuation $ continuation . (. f)

instance Covariant t => Pointable (Continuation r t) where
	point x = Continuation ($ x)

instance Covariant t => Applicative (Continuation r t) where
	f <*> x = Continuation $ \h -> run f $ \g -> run x # h . g

instance Covariant t => Bindable (Continuation r t) where
	x >>= f = Continuation $ \g -> run x $ \y -> run # f y # g

instance Monad t => Monad (Continuation r t) where

instance (forall u . Bindable u) => Liftable (Continuation r) where
	lift = Continuation . (>>=)

-- | Call with current continuation
cwcc :: ((a -> Continuation r t b) -> Continuation r t a) -> Continuation r t a
cwcc f = Continuation $ \g -> (run % g) . f $ Continuation . (!.) . g

-- | Delimit the continuation of any 'shift'
reset :: (forall u . Bindable u, Monad t, Traversable t) => Continuation r t r -> Continuation s t r
reset = lift . (run % point)

-- | Capture the continuation up to the nearest enclosing 'reset' and pass it
shift :: Pointable t => ((a -> t r) -> Continuation r t r) -> Continuation r t a
shift f = Continuation $ (run % point) . f

interruptable :: Pointable t => ((a -> Continuation a t a) -> Continuation a t a) -> t a
interruptable = (run % point) . cwcc
