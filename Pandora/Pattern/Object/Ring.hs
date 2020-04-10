module Pandora.Pattern.Object.Ring (Ring) where

import Pandora.Pattern.Object.Group (Group)

{- |
> When providing a new instance, you should ensure it satisfies the one law:
> * Commutativity of addition: x + y ≡ y + x
-}

class Group a => Ring a where
