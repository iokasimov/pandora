module Pandora.Pattern.Betwixt (Betwixt) where

type family Betwixt (source :: * -> * -> *) (target :: * -> * -> *) = (betwixt :: * -> * -> *) | betwixt -> source target
