{-# OPTIONS_GHC -Wno-orphans #-}

module RaylibUtil where

import FRP.Yampa (VectorSpace (..))
import Optics
import qualified Raylib.Types as RL
import qualified Raylib.Util as RL
import qualified Raylib.Util.Math as RL

instance VectorSpace RL.Vector2 Float where
  zeroVector = RL.zero
  (*^) = flip (RL.|*)
  (^+^) = (RL.|+|)
  dot = (RL.|.|)

instance Field1 RL.Vector2 RL.Vector2 Float Float

instance Field1 RL.Vector3 RL.Vector3 Float Float

instance Field1 RL.Vector4 RL.Vector4 Float Float

instance Field2 RL.Vector2 RL.Vector2 Float Float

instance Field2 RL.Vector3 RL.Vector3 Float Float

instance Field2 RL.Vector4 RL.Vector4 Float Float

instance Field3 RL.Vector3 RL.Vector3 Float Float

instance Field3 RL.Vector4 RL.Vector4 Float Float

instance Field4 RL.Vector4 RL.Vector4 Float Float

instance Show RL.WindowResources where
  show = const "WindowResources"