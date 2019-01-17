module Core.Transformer (type (:!:)) where

type (:!:) t u a = u (t a)
