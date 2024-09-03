{-# LANGUAGE PackageImports #-}

module Prelude (module P, module Optics, module Effectful) where

import Effectful
import Optics
-- optics has a more generic uncons
import "relude" Relude as P hiding (uncons)
