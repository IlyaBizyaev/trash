-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module FileSystem
  ( FileRevision(..)
  , File(..)
  , TrackerData(..)
  , DirEntry
  , Dir(..)
  , isDirTracked
  , listDirEntries
  , getDirEntryByFullPath
  , buildFileWithContent
  , emptyDir
  , findDirentsBySubstring
  , getChildCount
  , getFileMimeTypeByName
  , showOptionalTime
  , emptyTrackerData
  , calculateSize
  , getLogFromTrackerData
  , getRevisionFromTrackerData
  , listFilesInDirEntry
  , addRevisionsToTrackerData
  , removeRevisionFromTrackerData
  , removeFilesFromTrackerData
  , showPermissions
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text as TS
import Data.Time.Clock (UTCTime)
import Network.Mime
import System.Directory (Permissions, emptyPermissions, executable, readable, searchable, writable)
import System.FilePath.Posix

import PathUtils (fullNormalize)

data FileRevision = FileRevision {
  frGetName    :: String,
  frGetContent :: B.ByteString
}

instance Show FileRevision where
  show revision = summaryLine ++ '\n' : contentBlock   where
    summaryLine  = "Summary: " ++ frGetName revision
    contentBlock = BC.unpack $ frGetContent revision

defaultPermissions :: Permissions
defaultPermissions = emptyPermissions { readable = True, writable = True }

showPermissions :: Permissions -> String
showPermissions p = pRead : pWrite : [pExecute] where
  pRead    = if readable p then 'r' else '-'
  pWrite   = if writable p then 'w' else '-'
  pExecute = if (executable p || searchable p) then 'x' else '-'

data File = File {
  fGetPermissions      :: Permissions,
  fGetModificationTime :: Maybe UTCTime,
  fGetSize             :: Integer,
  fGetContent          :: B.ByteString
} deriving (Show)

data TrackerData = TrackerData {
  tGetLastVersion :: Integer,
  tGetRevisions   :: Map.Map String (Map.Map Integer FileRevision)
} deriving Show

addRevisionsToTrackerData
  :: TrackerData -> [(FilePath, FileRevision)] -> TrackerData
addRevisionsToTrackerData trackerData toAdd = TrackerData
  { tGetLastVersion = newLast
  , tGetRevisions   = newRevisions
  } where
  prevLast      = tGetLastVersion trackerData
  prevRevisions = tGetRevisions trackerData
  newLast       = prevLast + 1
  newRevisions  = foldl addOneRev prevRevisions toAdd
  addOneRev revsMap (path, rev) = case Map.lookup path revsMap of
    Nothing             -> Map.insert path (Map.singleton newLast rev) revsMap
    Just oldRevsForPath -> Map.insert path newRevsForPath revsMap
      where newRevsForPath = Map.insert newLast rev oldRevsForPath

getRevisionFromTrackerData
  :: TrackerData -> FilePath -> Integer -> Maybe FileRevision
getRevisionFromTrackerData td path rev = do
  numToRevMap <- Map.lookup path (tGetRevisions td)
  Map.lookup rev numToRevMap

getLogFromTrackerData
  :: TrackerData -> [FilePath] -> Maybe [(Integer, [FileRevision])]
getLogFromTrackerData td paths = do
  numToRevMaps <- mapM (\x -> Map.lookup x (tGetRevisions td)) paths
  let numToRevListMaps = map (Map.map (\x -> [x])) numToRevMaps
  let numToRevListMap  = foldl (Map.unionWith (++)) Map.empty numToRevListMaps
  return $ Map.toAscList numToRevListMap

removeRevisionFromTrackerData
  :: TrackerData -> FilePath -> Integer -> Maybe TrackerData
removeRevisionFromTrackerData td path rev = do
  let oldRevisions = tGetRevisions td
  oldRevsForPath <- Map.lookup path oldRevisions
  _              <- Map.lookup rev oldRevsForPath
  let newRevsForPath = Map.delete rev oldRevsForPath
  let newRevisions   = Map.insert path newRevsForPath oldRevisions
  return $ td { tGetRevisions = newRevisions }

removeFilesFromTrackerData :: TrackerData -> [FilePath] -> Maybe TrackerData
removeFilesFromTrackerData trackerData paths = foldl removeFile
                                                     (Just trackerData)
                                                     paths where
  removeFile maybeTd p = do
    td <- maybeTd
    let oldRevisions = tGetRevisions td
    _ <- Map.lookup p oldRevisions
    let newRevisions = Map.delete p oldRevisions
    return $ td { tGetRevisions = newRevisions }

data Dir = Dir {
  dGetTrackerData :: Maybe TrackerData,
  dGetPermissions :: Permissions,
  dGetSize        :: Integer,
  dGetChildren    :: Map.Map FilePath DirEntry
} deriving (Show)

emptyDir :: Dir
emptyDir = Dir { dGetTrackerData = Nothing
               , dGetPermissions = defaultPermissions
               , dGetSize        = 0
               , dGetChildren    = Map.empty
               }

emptyTrackerData :: TrackerData
emptyTrackerData =
  TrackerData { tGetLastVersion = 0, tGetRevisions = Map.empty }

type DirEntry = Either File Dir

buildFileWithContent :: String -> File
buildFileWithContent content = File { fGetPermissions      = defaultPermissions
                                    , fGetModificationTime = Nothing
                                    , fGetSize             = byteStringSize
                                    , fGetContent          = byteStringContent
                                    }
 where
  byteStringContent = BC.pack content
  byteStringSize    = (toInteger . B.length) byteStringContent

isDirTracked :: Dir -> Bool
isDirTracked dir = case dGetTrackerData dir of
  Nothing -> False
  _       -> True

listDirEntries :: Dir -> [FilePath]
listDirEntries dir = Map.keys (dGetChildren dir)

listFilesInDirEntry :: FilePath -> DirEntry -> [FilePath]
listFilesInDirEntry path (Left  _  ) = [path]
listFilesInDirEntry path (Right dir) = map (path </>) allFiles where
  children = Map.toAscList (dGetChildren dir)
  allFiles = concatMap (uncurry listFilesInDirEntry) children

getDirEntryByFullPath :: Dir -> FilePath -> Maybe DirEntry
getDirEntryByFullPath rootDir fullPath = getDirEntryByPathComponents
  rootDir
  pathComponents where
  normalizedPath = fullNormalize fullPath
  -- We omit "/", which is always present in split full paths
  pathComponents = tail $ splitDirectories normalizedPath
  getDirEntryByPathComponents dir []       = return $ Right dir
  getDirEntryByPathComponents dir (c : cs) = do
    childEntry <- Map.lookup c (dGetChildren dir)
    case childEntry of
      Left  f -> if null cs then return (Left f) else Nothing
      Right d -> getDirEntryByPathComponents d cs

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

calculateSize :: DirEntry -> DirEntry
calculateSize (Right dir) = Right $ dir { dGetSize = newSize } where
  children = Map.elems $ dGetChildren dir
  getSize (Left  f) = fGetSize f
  getSize (Right d) = dGetSize d
  childSizes = map getSize children
  newSize    = sum childSizes
calculateSize file = file
