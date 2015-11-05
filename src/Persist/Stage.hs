{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Persist.Stage where

import qualified Data.Aeson as Aeson
import           Database.Persist.Sql
import           Database.Persist.TH

derivePersistFieldJSON "Aeson.Value"
derivePersistFieldJSON "Aeson.Object"
