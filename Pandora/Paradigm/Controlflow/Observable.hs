module Pandora.Paradigm.Controlflow.Observable (Observable, observe,
	notify, follow, subscribe, watch, (.:~.), (.:~*), (*:~.), (*:~*)) where

import Pandora.Core.Functor (type (<))
import Pandora.Core.Interpreted ((<~~))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---))
import Pandora.Paradigm.Algebraic (Applicative, loop)
import Pandora.Paradigm.Primary.Transformer.Continuation (Continuation (Continuation))

newtype Capture r t a = Capture { captured :: t r }

type Observable t a r = Continuation r < Capture r t < a

-- | Make continuation observable
observe :: Continuation r t a -> Observable t a r
observe action = Continuation <-- \h ->
	Capture <--- action <~~ captured . h

-- | Listen only first event, call back just once
notify :: Observable t a r -> (a -> t r) -> t r
notify r action = captured <--- r <~~ Capture . action

-- | Infix version of 'notify'
(.:~.) :: Observable t a r -> (a -> t r) -> t r
(.:~.) = notify

-- | Listen only first event, call back loop
follow :: Applicative t => Observable t a r -> (a -> t r) -> t r
follow obs action = captured <--- obs <~~ Capture . loop . action

-- | Infix version of 'follow'
(.:~*) :: Applicative t => Observable t a r -> (a -> t r) -> t r
(.:~*) = follow

-- | Listen all events from action, call back just once
subscribe :: Applicative t => Observable t a r -> (a -> t r) -> t r
subscribe r action = loop <--- captured <--- r <~~ Capture . action

-- | Infix version of 'subscribe'
(*:~.) :: Applicative t => Observable t a r -> (a -> t r) -> t r
(*:~.) = subscribe

-- | Listen all events from action, call back loop
watch :: Applicative t => Observable t a r -> (a -> t r) -> t r
watch r action = loop <--- captured <--- r <~~ Capture . loop . action

-- | Infix version of 'watch'
(*:~*) :: Applicative t => Observable t a r -> (a -> t r) -> t r
(*:~*) = watch
