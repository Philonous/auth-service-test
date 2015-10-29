-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved


{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

module Persist.Schema where

import           Control.Lens
import           Data.Text (Text)
import           Data.Time.Clock
import           Database.Persist.Quasi
import           Database.Persist.TH

import           Types

share [ mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "src/schema")

makeLensesWith camelCaseFields ''User
makeLensesWith camelCaseFields ''UserInstance
makeLensesWith camelCaseFields ''Token
