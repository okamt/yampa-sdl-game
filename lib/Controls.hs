module Controls where

import Control.Monad.Representable.Reader (Representable)
import Data.Distributive
import Data.Distributive.Generic (genericCollect)
import Eff
import GHC.Generics
import RL

data Controls' a = Controls
  { upKey :: a,
    leftKey :: a,
    downKey :: a,
    rightKey :: a
  }
  deriving (Show, Functor, Foldable, Traversable, Generic1)

type Controls = Controls' RL.KeyboardKey

instance Distributive Controls' where collect = genericCollect

instance Representable Controls'

makeFieldLabelsNoPrefix ''Controls'

defaultControls :: Controls
defaultControls =
  Controls
    { upKey = RL.KeyW,
      leftKey = RL.KeyA,
      downKey = RL.KeyS,
      rightKey = RL.KeyD
    }

defaultControlsDev :: Controls
defaultControlsDev =
  Controls
    { upKey = RL.KeyL,
      leftKey = RL.KeyN,
      downKey = RL.KeyR,
      rightKey = RL.KeyT
    }

getDefaultControls :: (Eff.Environment :> es) => Eff es Controls
getDefaultControls =
  Eff.lookupEnv "DEV"
    <&> \case
      Nothing -> defaultControls
      Just _ -> defaultControlsDev