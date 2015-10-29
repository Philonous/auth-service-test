-- Copyright 2015 Lambdatrade AB
-- All rights reserved

module Helpers where

import           Data.Char
import qualified Data.List as List

dropPrefix :: [Char] -> [Char] -> [Char]
dropPrefix pre str=
    case pre `List.stripPrefix` str of
     Just (n:ns) -> toLower n : ns
     _ -> error "prefix now found"
