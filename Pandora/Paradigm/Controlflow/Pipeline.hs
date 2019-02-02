module Pandora.Paradigm.Controlflow.Pipeline (Pipeline, await, yield, finish, impact) where

import Pandora.Core.Morphism (($))
import Pandora.Paradigm.Basis.Continuation (Continuation (Continuation, continue))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Pointable (Pointable (point))
import Pandora.Pattern.Functor.Bindable (Bindable ((>>=)))

newtype Producer i t r = Producer { produce :: Consumer i t r -> t r }

newtype Consumer o t r = Consumer { consume :: o -> Producer o t r -> t r }

newtype Pipe i o r t a = Pipe { pipe :: Producer i t r -> Consumer o t r -> t r }

instance Covariant (Pipe i o r t) where
	f <$> Pipe p = Pipe p

instance Contravariant (Pipe i o r t) where
	f >$< Pipe p = Pipe p

type Pipeline i o t a r = Continuation r (Pipe i o r t) a

pause :: (() -> Pipe i o r t a) -> Producer i t r -> Producer o t r
pause next ik = Producer $ \ok -> (pipe $ next ()) ik ok

suspend :: (i -> Pipe i o r t a) -> Consumer o t r -> Consumer i t r
suspend next ok = Consumer $ \v ik -> pipe (next v) ik ok

await :: Pipeline i o t i r
await = Continuation $ \next -> Pipe $ \i o -> produce i (suspend next o)

yield :: o -> Pipeline i o t () r
yield v = Continuation $ \next -> Pipe $ \i o -> consume o v (pause next i)

finish :: Pointable t => Pipeline i o t () ()
finish = Continuation $ \_ -> Pipe $ \_ _ -> point ()

impact :: Bindable t => t a -> Pipeline i o t a a
impact action = Continuation $ \next -> Pipe $ \i o -> action >>= \x -> pipe (next x) i o
