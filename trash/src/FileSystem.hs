-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module FileSystem
  ( FileRevision(..)
  , File(..)
  , TrackerData(..)
  , DirEntry
  , Dir(..)
  , readDirFromFilesystem
  , writeDirToFilesystem
  , isDirTracked
  , listDirEntries
  , getDirentryByFullPath
  , buildFileWithContent
  , addDirEntry
  , rmDirEntry
  , emptyDir
  , isFileTrackedInDir
  , findDirentsBySubstring
  , getChildCount
  , getFileMimeTypeByName
  , showOptionalTime
  )
where

import           System.Directory               ( Permissions
                                                , emptyPermissions
                                                )
import           Data.Time.Clock                ( UTCTime )
import qualified Data.Map.Strict               as Map
import           System.FilePath.Posix
import           Network.Mime
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.List                      ( isInfixOf )
import qualified Data.Text                     as TS

data FileRevision = FileRevision {
  frGetVersion :: Int,
  frGetName :: String,
  frGetTimestamp :: UTCTime,
  frGetContent :: B.ByteString
}

-- TODO: how to reliably set permissions for the file that we are going to create? Do we know them in advance?
data File = File {
  fGetPermissions :: Permissions,
  fGetModificationTime :: Maybe UTCTime,
  fGetCreationTime :: Maybe UTCTime,
  fGetSize :: Int,
  fGetContent :: B.ByteString
}

data TrackerData = TrackerData {
  tGetLastVersion :: Integer,
  tGetRevisions :: Map.Map String (Map.Map Integer FileRevision)
}

-- TODO: delete file count?
data Dir = Dir {
  dGetTrackerData :: Maybe TrackerData,
  dGetPermissions :: Permissions,
  dGetSize :: Integer,
  dGetChildren :: Map.Map FilePath DirEntry
}

emptyDir :: Dir
emptyDir = Dir { dGetTrackerData = Nothing
               , dGetPermissions = emptyPermissions
               , dGetSize        = 0
               , dGetChildren    = Map.empty
               }

type DirEntry = Either File Dir

-- TODO: learn to properly pack the content, maybe for another input format (Text?)
-- TODO: and verify that size measuring makes sense
buildFileWithContent :: String -> File
buildFileWithContent content = File { fGetPermissions      = defaultPermissions
                                    , fGetModificationTime = Nothing
                                    , fGetCreationTime     = Nothing
                                    , fGetSize             = byteStringSize
                                    , fGetContent          = byteStringContent
                                    }
 where
  byteStringContent  = BC.pack content
  byteStringSize     = B.length byteStringContent
  defaultPermissions = emptyPermissions

isDirTracked :: Dir -> Bool
isDirTracked dir = case dGetTrackerData dir of
  Nothing -> False
  _       -> True

listDirEntries :: Dir -> [FilePath]
listDirEntries dir = Map.keys (dGetChildren dir)

addDirEntry :: Dir -> DirEntry -> FilePath -> Dir
addDirEntry = undefined
-- terrible note: this has to update sizes of all parent dirs

rmDirEntry :: Dir -> FilePath -> Dir
rmDirEntry = undefined
-- and this, too
-- and even worse, both of these can fail, so should mb return Maybe

getDirentryByFullPath :: Dir -> FilePath -> Maybe DirEntry
getDirentryByFullPath = undefined

readDirFromFilesystem :: FilePath -> IO Dir
readDirFromFilesystem path = undefined

writeDirToFilesystem :: Dir -> IO ()
writeDirToFilesystem dir = undefined

isFileTrackedInDir :: Dir -> FilePath -> Bool
isFileTrackedInDir = undefined

findDirentsBySubstring :: String -> DirEntry -> [FilePath]
findDirentsBySubstring query dirent = case dirent of
  Left  _   -> []
  Right dir -> matchingNames ++ recursiveRes   where
    children     = dGetChildren dir
    childrenList = Map.toAscList children
    namePredicate name = query `isInfixOf` name
    dirPredicate (_, de) = case de of
      Right _ -> True
      _       -> False
    matchingNames = filter namePredicate (Map.keys children)
    subdirs       = filter dirPredicate childrenList
    recCall (p, d) = map (p </>) (findDirentsBySubstring query d)
    recursiveRes = concatMap recCall subdirs

getChildCount :: Dir -> Int
getChildCount d = Map.size $ dGetChildren d

getFileMimeTypeByName :: String -> String
getFileMimeTypeByName name = BC.unpack $ defaultMimeLookup $ TS.pack name

showOptionalTime :: Maybe UTCTime -> String
showOptionalTime mbTime = case mbTime of
  Just t -> show t
  _      -> "-"
