{-# OPTIONS_GHC -Wno-orphans #-}

module SDLUtil where

import FRP.Yampa (VectorSpace (..))
import Optics
import qualified SDL

instance Field1 (SDL.V1 a) (SDL.V1 a) a a

instance Field1 (SDL.V2 a) (SDL.V2 a) a a

instance Field1 (SDL.V3 a) (SDL.V3 a) a a

instance Field1 (SDL.V4 a) (SDL.V4 a) a a

instance Field2 (SDL.V2 a) (SDL.V2 a) a a

instance Field2 (SDL.V3 a) (SDL.V3 a) a a

instance Field2 (SDL.V4 a) (SDL.V4 a) a a

instance Field3 (SDL.V3 a) (SDL.V3 a) a a

instance Field3 (SDL.V4 a) (SDL.V4 a) a a

instance Field4 (SDL.V4 a) (SDL.V4 a) a a

instance (Eq a, Floating a, SDL.Metric v) => VectorSpace (v a) a where
  zeroVector = SDL.zero
  (*^) = (SDL.*^)
  (^+^) = (SDL.^+^)
  dot = SDL.dot

mkPoint :: a -> a -> SDL.Point SDL.V2 a
mkPoint x y = SDL.P (SDL.V2 x y)

mkRect :: a -> a -> a -> a -> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle (mkPoint x y) (SDL.V2 w h)