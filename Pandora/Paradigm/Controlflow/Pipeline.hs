module Pandora.Paradigm.Controlflow.Pipeline (Pipeline, await, yield, finish, impact, (=*=), pipeline) where

import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Monoidal (Monoidal)
import Pandora.Paradigm.Primary.Algebraic.Exponential (type (-->))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:))
import Pandora.Paradigm.Primary.Algebraic (point)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite, (!)))
import Pandora.Paradigm.Primary.Transformer.Continuation (Continuation (Continuation))

newtype Producer i t r = Producer { produce :: Consumer i t r -> t r }

instance Interpreted (->) (Producer i t) where
	type Primary (Producer i t) a = Consumer i t a -> t a
	run ~(Producer f) = f
	unite = Producer

newtype Consumer o t r = Consumer { consume :: o -> Producer o t r -> t r }

instance Interpreted (->) (Consumer o t) where
	type Primary (Consumer o t) a = o -> Producer o t a -> t a
	run ~(Consumer f) = f
	unite = Consumer

newtype Pipe i o r t a = Pipe { pipe :: Producer i t r -> Consumer o t r -> t r }

type Pipeline i o t a r = Continuation r (Pipe i o r t) a

pause :: (() -> Pipe i o r t a) -> Producer i t r -> Producer o t r
pause next ik = Producer ! \ok -> (pipe ! next ()) ik ok

suspend :: (i -> Pipe i o r t a) -> Consumer o t r -> Consumer i t r
suspend next ok = Consumer ! \v ik -> pipe <-- next v <-- ik <-- ok

-- | Take incoming value from pipeline
await :: Pipeline i o t i r
await = Continuation ! \next -> Pipe ! \(Producer i) o -> i <-- suspend next o

-- | Give a value to the future consuming
yield :: o -> Pipeline i o t () r
yield v = Continuation ! \next -> Pipe ! \i (Consumer o) -> o v <-- pause next i

-- | Pipeline that does nothing
finish :: Monoidal (-->) (-->) (:*:) (:*:) t => Pipeline i o t () ()
finish = Continuation (constant <-- Pipe (constant . constant <-- point ()))

-- | Do some effectful computation within pipeline
impact :: Bindable (->) t => t a -> Pipeline i o t a a
impact action = Continuation ! \next -> Pipe ! \i o -> (\x -> pipe (next x) i o) =<< action

-- | Compose two pipelines into one
(=*=) :: forall i e o t . Monoidal (-->) (-->) (:*:) (:*:) t => Pipeline i e t () () -> Pipeline e o t () () -> Pipeline i o t () ()
p =*= q = Continuation ! \_ -> Pipe ! \i -> pipe <-- run q end <-- pause (constant <-- run p end) i where

	end :: b -> Pipe c d () t ()
	end _ = Pipe (constant . constant <-- point ())

-- | Run pipeline and get result
pipeline :: Monoidal (-->) (-->) (:*:) (:*:) t => Pipeline i o t () () -> t ()
pipeline p = pipe <-- run p (Pipe . constant . constant . point) <-- i <-- o where

	i :: Producer i t ()
	i = Producer ! \o' -> produce i o'

	o :: Consumer o t ()
	o = Consumer ! \v i' -> consume o v i'
