module Pandora.Paradigm.Basis (module Exports, note, hush) where

import Pandora.Paradigm.Basis.Twister as Exports
import Pandora.Paradigm.Basis.Free as Exports
import Pandora.Paradigm.Basis.Fix as Exports
import Pandora.Paradigm.Basis.Yoneda as Exports
import Pandora.Paradigm.Basis.Continuation as Exports
import Pandora.Paradigm.Basis.Predicate as Exports
import Pandora.Paradigm.Basis.Variation as Exports
import Pandora.Paradigm.Basis.Wye as Exports
import Pandora.Paradigm.Basis.Edges as Exports
import Pandora.Paradigm.Basis.Conclusion as Exports
import Pandora.Paradigm.Basis.Maybe as Exports
import Pandora.Paradigm.Basis.Endo as Exports
import Pandora.Paradigm.Basis.Jet as Exports
import Pandora.Paradigm.Basis.Jack as Exports
import Pandora.Paradigm.Basis.Product as Exports
import Pandora.Paradigm.Basis.Constant as Exports
import Pandora.Paradigm.Basis.Identity as Exports

import Pandora.Core.Functor (type (~>))
import Pandora.Core.Morphism ((!))

note :: e -> Maybe ~> Conclusion e
note x = maybe (Failure x) Success

hush :: Conclusion e ~> Maybe
hush = conclusion (Nothing !) Just
