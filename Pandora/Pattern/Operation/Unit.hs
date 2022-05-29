module Pandora.Pattern.Operation.Unit where

type family Unit (p :: * -> * -> *) = r | r -> p
