module Pandora.Paradigm.Controlflow.Observable (Observable, observe,
	notify, follow, subscribe, watch, (.:~.), (.:~*), (*:~.), (*:~*)) where

import Pandora.Core.Morphism ((.), ($))
import Pandora.Paradigm.Basis.Capture (Capture (Capture, captured))
import Pandora.Paradigm.Basis.Continuation (Continuation (Continuation, continue))
import Pandora.Pattern.Functor.Applicative (Applicative (forever))

type Observable t a r = Continuation r (Capture r t) a

-- | Make continuation observable
observe :: Continuation r t a -> Observable t a r
observe f = Continuation $ \h -> Capture $ continue f (captured . h)

-- | Listen only first event, call back just once
notify :: Observable t a r -> (a -> t r) -> t r
notify r f = captured $ continue r (Capture . f)

-- | Infix version of 'notify'
(.:~.) :: Observable t a r -> (a -> t r) -> t r
(.:~.) = notify

-- | Listen only first event, call back forever
follow :: Applicative t => Observable t a r -> (a -> t r) -> t r
follow r f = captured $ continue r (Capture . forever . f)

-- | Infix version of 'follow'
(.:~*) :: Applicative t => Observable t a r -> (a -> t r) -> t r
(.:~*) = follow

-- | Listen all events from action, call back just once
subscribe :: Applicative t => Observable t a r -> (a -> t r) -> t r
subscribe r f = forever $ captured $ continue r (Capture . f)

-- | Infix version of 'subscribe'
(*:~.) :: Applicative t => Observable t a r -> (a -> t r) -> t r
(*:~.) = subscribe

-- | Listen all events from action, call back forever
watch :: Applicative t => Observable t a r -> (a -> t r) -> t r
watch r f = forever $ captured $ continue r (Capture . forever . f)

-- | Infix version of 'watch'
(*:~*) :: Applicative t => Observable t a r -> (a -> t r) -> t r
(*:~*) = watch
