-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module PathUtils
  ( fullNormalize
  , makeRelativeTo
  , isChildOfPath
  , lastSegment
  )
where

import Data.List (isPrefixOf, stripPrefix)
import System.FilePath.Posix

fullNormalize :: FilePath -> FilePath
fullNormalize = normalise -- TODO: take .. into account

makeRelativeTo :: FilePath -> FilePath -> Maybe FilePath
makeRelativeTo parentPath absolutePath = do
  suffix <- stripPrefix parentSplit childSplit
  return $ joinPath suffix
 where
  parentSplit = splitDirectories parentPath
  childSplit  = splitDirectories absolutePath

isChildOfPath :: FilePath -> FilePath -> Bool
isChildOfPath parentPath absolutePath = parentSplit `isPrefixOf` childSplit where
  parentSplit = splitDirectories parentPath
  childSplit  = splitDirectories absolutePath

lastSegment :: FilePath -> FilePath
lastSegment = takeFileName . dropTrailingPathSeparator
