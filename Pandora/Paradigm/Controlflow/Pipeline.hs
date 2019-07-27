module Pandora.Paradigm.Controlflow.Pipeline (Pipeline, await, yield, finish, impact, (=*=), pipeline) where

import Pandora.Paradigm.Basis.Continuation (Continuation (Continuation, continue))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))
import Pandora.Pattern.Functor.Divariant (($))

newtype Producer i t r = Producer { produce :: Consumer i t r -> t r }

newtype Consumer o t r = Consumer { consume :: o -> Producer o t r -> t r }

newtype Pipe i o r t a = Pipe { pipe :: Producer i t r -> Consumer o t r -> t r }

instance Covariant (Pipe i o r t) where
	_ <$> Pipe p = Pipe p

instance Contravariant (Pipe i o r t) where
	_ >$< Pipe p = Pipe p

type Pipeline i o t a r = Continuation r (Pipe i o r t) a

pause :: (() -> Pipe i o r t a) -> Producer i t r -> Producer o t r
pause next ik = Producer $ \ok -> (pipe $ next ()) ik ok

suspend :: (i -> Pipe i o r t a) -> Consumer o t r -> Consumer i t r
suspend next ok = Consumer $ \v ik -> pipe (next v) ik ok

-- | Take incoming value from pipeline
await :: Pipeline i o t i r
await = Continuation $ \next -> Pipe $ \i o -> produce i (suspend next o)

-- | Give a value to the future consuming
yield :: o -> Pipeline i o t () r
yield v = Continuation $ \next -> Pipe $ \i o -> consume o v (pause next i)

-- | Pipeline that does nothing
finish :: Pointable t => Pipeline i o t () ()
finish = Continuation $ \_ -> Pipe $ \_ _ -> point ()

-- | Do some effectful computation within pipeline
impact :: Bindable t => t a -> Pipeline i o t a a
impact action = Continuation $ \next -> Pipe $ \i o -> action >>= \x -> pipe (next x) i o

-- | Compose two pipelines into one
(=*=) :: forall i e a o t . Pointable t => Pipeline i e t () () -> Pipeline e o t () () -> Pipeline i o t a ()
p =*= q = Continuation $ \_ -> Pipe $ \i o -> pipe (continue q end) (pause (\() -> continue p end) i) o where

	end :: b -> Pipe c d () t ()
	end _ = Pipe $ \_ _ -> point ()

-- | Run pipeline and get result
pipeline :: Pointable t => Pipeline i o t r r -> t r
pipeline p = pipe (continue p (\r -> Pipe $ \_ _ -> point r)) i o where

	i :: Producer i t r
	i = Producer $ \o' -> produce i o'

	o :: Consumer o t r
	o = Consumer $ \v i' -> consume o v i'
