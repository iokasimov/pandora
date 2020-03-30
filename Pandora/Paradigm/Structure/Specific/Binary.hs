{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Specific.Binary (Binary, insert) where

import Pandora.Core.Morphism ((&), (%))
import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (<$$>)))
import Pandora.Pattern.Functor.Traversable (Traversable ((->>), (->>>)))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Pattern.Object.Chain (Chain ((<=>)), order)
import Pandora.Paradigm.Basis.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Basis.Wye (Wye (End, Left, Right, Both))
import Pandora.Paradigm.Basis.Twister (Twister (Twister))
import Pandora.Paradigm.Controlflow.Joint.Schemes.UT (UT (UT))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (run)

type Binary = UT Covariant Covariant (Twister Wye) Maybe

instance Covariant Binary where
	f <$> UT g = UT $ f <$$> g

instance Traversable Binary where
	UT g ->> f = UT <$> g ->>> f

insert :: Chain a => a -> Binary a -> Binary a
insert x (UT Nothing) = UT $ Just $ Twister x End
insert x (UT (Just (Twister y End))) = x <=> y & order
	(UT . Just . Twister y . Right $ Twister x End)
	(UT . Just . Twister y . Right $ Twister x End)
	(UT . Just . Twister y . Left $ Twister x End)
insert x (UT (Just (Twister y (Left ls)))) = x <=> y & order
	(UT . Just . Twister y . Both ls $ Twister x End)
	(UT . Just . Twister y $ Both ls $ Twister x End)
	(UT $ Twister y . Left <$> run (insert x . UT . Just $ ls))
insert x (UT (Just (Twister y (Right rs)))) = x <=> y & order
	(UT $ Twister y . Right <$> run (insert x . UT . Just $ rs))
	(UT $ Twister y . Right <$> run (insert x . UT . Just $ rs))
	(UT . Just . Twister y $ Both (Twister x End) rs)
insert x (UT (Just (Twister y (Both ls rs)))) = x <=> y & order
	(UT $ Twister y . Both ls <$> run (insert x . UT . Just $ rs))
	(UT $ Twister y . Both ls <$> run (insert x . UT . Just $ rs))
	(UT $ Twister y . Both % rs <$> (run . insert x . UT . Just $ ls))
