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

-- | Apply full normalization (get rid of . and .. links).
-- TODO: take .. into account
fullNormalize :: FilePath -> FilePath
fullNormalize = normalise

-- | Make path relative to its parent, or fail if not a child.
makeRelativeTo :: FilePath -> FilePath -> Maybe FilePath
makeRelativeTo parentPath absolutePath = do
  suffix <- stripPrefix parentSplit childSplit
  return $ joinPath suffix
 where
  parentSplit = splitDirectories parentPath
  childSplit  = splitDirectories absolutePath

-- | Check if given path is a child of (or equal to) provided parent path.
isChildOfPath :: FilePath -> FilePath -> Bool
isChildOfPath parentPath absolutePath = parentSplit `isPrefixOf` childSplit where
  parentSplit = splitDirectories parentPath
  childSplit  = splitDirectories absolutePath

-- | Get last segment of the provided path.
lastSegment :: FilePath -> FilePath
lastSegment = takeFileName . dropTrailingPathSeparator
