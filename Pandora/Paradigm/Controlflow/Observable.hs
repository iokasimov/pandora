module Pandora.Paradigm.Controlflow.Observable (Observable, observe,
	notify, follow, subscribe, watch, (.:~.), (.:~*), (*:~.), (*:~*)) where

import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Applicative (Applicative (forever))
import Pandora.Paradigm.Primary.Transformer.Continuation (Continuation (Continuation))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)

newtype Capture r t a = Capture { captured :: t r }

type Observable t a r = Continuation r (Capture r t) a

-- | Make continuation observable
observe :: Continuation r t a -> Observable t a r
observe action = Continuation $ \h -> Capture $ run action (captured . h)

-- | Listen only first event, call back just once
notify :: Observable t a r -> (a -> t r) -> t r
notify r action = captured $ run r (Capture . action)

-- | Infix version of 'notify'
(.:~.) :: Observable t a r -> (a -> t r) -> t r
(.:~.) = notify

-- | Listen only first event, call back forever
follow :: Applicative t => Observable t a r -> (a -> t r) -> t r
follow r action = captured $ run r (Capture . forever . action)

-- | Infix version of 'follow'
(.:~*) :: Applicative t => Observable t a r -> (a -> t r) -> t r
(.:~*) = follow

-- | Listen all events from action, call back just once
subscribe :: Applicative t => Observable t a r -> (a -> t r) -> t r
subscribe r action = forever $ captured $ run r (Capture . action)

-- | Infix version of 'subscribe'
(*:~.) :: Applicative t => Observable t a r -> (a -> t r) -> t r
(*:~.) = subscribe

-- | Listen all events from action, call back forever
watch :: Applicative t => Observable t a r -> (a -> t r) -> t r
watch r action = forever $ captured $ run r (Capture . forever . action)

-- | Infix version of 'watch'
(*:~*) :: Applicative t => Observable t a r -> (a -> t r) -> t r
(*:~*) = watch
