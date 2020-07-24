{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Persist.Migration
  ( Persist.Migration.migrate
  ) where

import NejlaCommon.Persistence.Migration as M

migrations =
  [ Migration { expect = "" -- No migrations present
              , to = "1"
              , description = "Initial version"
              , script = do
                  schemaEmptyP "public" >>= \case
                    True -> do -- Database not initialized at all
                      rawExecute $(sqlFile "src/Persist/migrations/00-initial.sql") []
                    False -> -- Database _was_ initialized, but schema
                             -- versionioning wasn't in use
                      return ()
              }
  , Migration { expect = "1"
              , to = "2"
              , description = "Case insenstivie email addresses"
              , script = rawExecute
                           $(sqlFile "src/Persist/migrations/01-ci-emails.sql") []
              }
  ]

migrate = M.migrate $(gitHash) migrations
