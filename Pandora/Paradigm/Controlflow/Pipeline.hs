module Pandora.Paradigm.Controlflow.Pipeline (Pipeline, await, yield) where

import Pandora.Core.Morphism (($))

import Pandora.Paradigm.Basis.Continuation (Continuation (Continuation, continue))

newtype Producer i t r = Producer { produce :: Consumer i t r -> t r }

newtype Consumer o t r = Consumer { consume :: o -> Producer o t r -> t r }

newtype Pipe i o r t a = Pipe { pipe :: Producer i t r -> Consumer o t r -> t r }

type Pipeline i o t a r = Continuation r (Pipe i o r t) a

pause :: (() -> Pipe i o r t a) -> Producer i t r -> Producer o t r
pause next ik = Producer $ \ok -> (pipe $ next ()) ik ok

suspend :: (i -> Pipe i o r t a) -> Consumer o t r -> Consumer i t r
suspend next ok = Consumer $ \v ik -> pipe (next v) ik ok

await :: Pipeline i o t i r
await = Continuation $ \next -> Pipe $ \i o -> produce i (suspend next o)

yield :: o -> Pipeline i o t () r
yield v = Continuation $ \next -> Pipe $ \i o -> consume o v (pause next i)
