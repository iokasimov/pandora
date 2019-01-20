module Pandora.Paradigm.Basis.Transformations where

import Pandora.Core.Morphism ((.), (!))
import Pandora.Core.Transformation (type (~>))
import Pandora.Paradigm.Basis.Product
import Pandora.Paradigm.Basis.Conclusion (Conclusion (Failure, Success), conclusion)
import Pandora.Paradigm.Basis.Maybe (Maybe (Just, Nothing), maybe)

note :: e -> Maybe ~> Conclusion e
note x = maybe (Failure x) Success

hush :: Conclusion e ~> Maybe
hush = conclusion (Nothing !) Just
