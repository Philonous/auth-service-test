{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}

module Util where

import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Microstache           as Mustache
import Instances.TH.Lift ()

-- Orphan instances for Microstache Templates

deriving instance TH.Lift Mustache.PName
deriving instance TH.Lift Mustache.Key
deriving instance TH.Lift Mustache.Node
deriving instance TH.Lift Mustache.Template
