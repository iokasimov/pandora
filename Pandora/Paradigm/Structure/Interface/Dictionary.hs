{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Interface.Dictionary where

import Pandora.Pattern.Category ((#))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Schemes.TU (type (<:.>))

import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing), Morph (Lookup), morph)

type Dictionary f t = Morphable (Lookup f) t

lookup :: forall f k t u a . (Dictionary f t, Morphing (Lookup f) t ~ ((->) k <:.> u)) => k -> t a -> u a
lookup key xs = run # morph @(Lookup f) xs # key

discover :: forall f k v t u a . (Dictionary f t, Morphing (Lookup f) t ~ ((->) (v k) <:.> u)) => v k -> t a -> u a
discover keys xs = run # morph @(Lookup f) xs # keys
