module Pandora.Paradigm.Structure.Specific.Binary (Binary, insert) where

import Pandora.Core.Morphism ((&))
import Pandora.Paradigm.Basis.Wye (Wye (End, Left, Right, Both))
import Pandora.Paradigm.Basis.Twister (Twister (Twister))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Object.Chain (Chain ((<=>)), order)

type Binary = Twister Wye

insert :: Chain a => a -> Binary a -> Binary a
insert x (Twister y End) = x <=> y & order (Twister y . Right $ Twister x End) (Twister y . Right $ Twister x End) (Twister y . Left $ Twister x End)
insert x (Twister y (Left ls)) = x <=> y & order (Twister y . Both ls $ Twister x End) (Twister y $ Both ls $ Twister x End) (Twister y . Left $ insert x ls)
insert x (Twister y (Right rs)) = x <=> y & order (Twister y . Right $ insert x rs) (Twister y $ Right (insert x rs)) (Twister y $ Both (Twister x End) rs)
insert x (Twister y (Both ls rs)) = x <=> y & order (Twister y . Both ls $ insert x rs) (Twister y . Both ls $ insert x rs) (Twister y $ Both (insert x ls) rs)
