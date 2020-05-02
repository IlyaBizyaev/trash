-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module FileSystem
  ( FileRevision(..)
  , File(..)
  , TrackerData(..)
  , Dir(..)
  , readDirFromFilesystem
  , writeDirToFilesystem
  , isDirTracked
  , getDirentryByPath
  )
where

import           System.Directory               ( Permissions )
import           Data.Time.Clock                ( UTCTime )
import qualified Data.Map.Strict               as Map

data FileRevision = FileRevision {
  frGetVersion :: Int,
  frGetName :: String,
  frGetTimestamp :: UTCTime,
  frGetContent :: String
}

-- TODO: how to reliably set permissions for the file that we are going to create? Do we know them in advance?
data File = File {
  fGetName :: String,
  fGetPath :: FilePath,
  fGetMimeType :: String,
  fGetPermissions :: Permissions,
  fGetModificationTime :: Maybe UTCTime,
  fGetCreationTime :: Maybe UTCTime,
  fGetSize :: Integer,
  fGetContent :: String
}

data TrackerData = TrackerData {
  tGetLastVersion :: Integer,
  tGetRevisions :: Map.Map String (Map.Map Integer FileRevision)
}

data Dir = Dir {
  dGetTrackerData :: Maybe TrackerData,
  dGetName :: String,
  dGetPath :: FilePath,
  dGetPermissions :: Permissions,
  dGetFileCount :: Integer,
  dGetSize :: Integer,
  dGetChildren :: Map.Map FilePath DirEntry
}

type DirEntry = Either File Dir

isDirTracked :: Dir -> Bool
isDirTracked dir = case dGetTrackerData dir of
  Nothing -> False
  _       -> True


getDirentryByPath :: Dir -> FilePath -> Either String DirEntry
getDirentryByPath dir path = undefined -- split the path in sections, map all the way down, ensure it's a dir

readDirFromFilesystem :: FilePath -> IO Dir
readDirFromFilesystem path = undefined

writeDirToFilesystem :: Dir -> IO ()
writeDirToFilesystem dir = undefined
