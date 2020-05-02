-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module FileSystem
  (FileRevision(..), File(..), Dir(..))
where

import System.Directory (Permissions)
import Data.Time.Clock (UTCTime)

data FileRevision = FileRevision {
  frGetTimestamp :: UTCTime,
  frGetName :: String,
  frGetContent :: String,
  frGetVersion :: Int
}

data File = File {
  fGetName :: String,
  fGetPath :: FilePath,
  fGetMimeType :: String,
  fGetPermissions :: Permissions,
  fGetModificationTime :: UTCTime,
  fGetCreationTime :: UTCTime,
  fGetSize :: Integer,
  fGetContent :: String,
  fGetRevisions :: [FileRevision]
}

data Dir = Dir {
  dGetName :: String,
  dGetPath :: FilePath,
  dGetPermissions :: Permissions,
  dGetFileCount :: Integer,
  dGetSize :: Integer,
  dGetChildren :: DirEntry
}

type DirEntry = Either File Dir
