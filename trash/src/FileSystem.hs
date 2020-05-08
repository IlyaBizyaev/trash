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

-- | Data type to store committed file revisions.
data FileRevision = FileRevision {
  frGetName    :: String,
  frGetContent :: B.ByteString
}

instance Show FileRevision where
  show revision = summaryLine ++ '\n' : contentBlock   where
    summaryLine  = "Summary: " ++ frGetName revision
    contentBlock = BC.unpack $ frGetContent revision

-- | Default permissions for files and directories (rw-).
defaultPermissions :: Permissions
defaultPermissions = emptyPermissions { readable = True, writable = True }

-- | Pretty-print permissions.
showPermissions :: Permissions -> String
showPermissions p = pRead : pWrite : [pExecute] where
  pRead    = if readable p then 'r' else '-'
  pWrite   = if writable p then 'w' else '-'
  pExecute = if (executable p || searchable p) then 'x' else '-'

-- | Data type for files in memory.
data File = File {
  fGetPermissions      :: Permissions,
  fGetModificationTime :: Maybe UTCTime,
  fGetSize             :: Integer,
  fGetContent          :: B.ByteString
} deriving (Show)

-- | Data type for per-directory tracker data.
data TrackerData = TrackerData {
  tGetLastVersion :: Integer,
  tGetRevisions   :: Map.Map String (Map.Map Integer FileRevision)
} deriving Show

-- | Add specified revisions to tracker data.
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

-- | Get revision from tracker data by path and version number.
getRevisionFromTrackerData
  :: TrackerData -> FilePath -> Integer -> Maybe FileRevision
getRevisionFromTrackerData td path rev = do
  numToRevMap <- Map.lookup path (tGetRevisions td)
  Map.lookup rev numToRevMap

-- | Present revisions of the specified files as a loggable structure.
getLogFromTrackerData
  :: TrackerData -> [FilePath] -> Maybe [(Integer, [FileRevision])]
getLogFromTrackerData td paths = do
  numToRevMaps <- mapM (\x -> Map.lookup x (tGetRevisions td)) paths
  let numToRevListMaps = map (Map.map (\x -> [x])) numToRevMaps
  let numToRevListMap  = foldl (Map.unionWith (++)) Map.empty numToRevListMaps
  return $ Map.toAscList numToRevListMap

-- | Remove specified revisions from tracker data.
removeRevisionFromTrackerData
  :: TrackerData -> FilePath -> Integer -> Maybe TrackerData
removeRevisionFromTrackerData td path rev = do
  let oldRevisions = tGetRevisions td
  oldRevsForPath <- Map.lookup path oldRevisions
  _              <- Map.lookup rev oldRevsForPath
  let newRevsForPath = Map.delete rev oldRevsForPath
  let newRevisions   = Map.insert path newRevsForPath oldRevisions
  return $ td { tGetRevisions = newRevisions }

-- | Remove specified files (all their revisions) from tracker data.
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

-- | Data type for directories in memory.
data Dir = Dir {
  dGetTrackerData :: Maybe TrackerData,
  dGetPermissions :: Permissions,
  dGetSize        :: Integer,
  dGetChildren    :: Map.Map FilePath DirEntry
} deriving (Show)

-- | Empty directory (initial state after creation).
emptyDir :: Dir
emptyDir = Dir { dGetTrackerData = Nothing
               , dGetPermissions = defaultPermissions
               , dGetSize        = 0
               , dGetChildren    = Map.empty
               }

-- | Empty tracker data (initial state after init).
emptyTrackerData :: TrackerData
emptyTrackerData =
  TrackerData { tGetLastVersion = 0, tGetRevisions = Map.empty }

-- | Data type for directory entries (files / directories).
type DirEntry = Either File Dir

-- | Initialize File with provided text content.
buildFileWithContent :: String -> File
buildFileWithContent content = File { fGetPermissions      = defaultPermissions
                                    , fGetModificationTime = Nothing
                                    , fGetSize             = byteStringSize
                                    , fGetContent          = byteStringContent
                                    }
 where
  byteStringContent = BC.pack content
  byteStringSize    = (toInteger . B.length) byteStringContent

-- | Check if the specified directory contains tracker data.
isDirTracked :: Dir -> Bool
isDirTracked dir = case dGetTrackerData dir of
  Nothing -> False
  _       -> True

-- | List dir's direct children.
listDirEntries :: Dir -> [FilePath]
listDirEntries dir = Map.keys (dGetChildren dir)

-- | List all children of dirent (recursively).
listFilesInDirEntry :: FilePath -> DirEntry -> [FilePath]
listFilesInDirEntry path (Left  _  ) = [path]
listFilesInDirEntry path (Right dir) = map (path </>) allFiles where
  children = Map.toAscList (dGetChildren dir)
  allFiles = concatMap (uncurry listFilesInDirEntry) children

-- | Extract dirent from given dir by its full path.
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

-- | Use recursive substring search to find matching dirent paths.
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

-- | Count direct directory children.
getChildCount :: Dir -> Int
getChildCount d = Map.size $ dGetChildren d

-- | Approximate file type (MIME) based on the extension.
-- TODO: use libmagic bindings to rely on content-based detection.
getFileMimeTypeByName :: String -> String
getFileMimeTypeByName name = BC.unpack $ defaultMimeLookup $ TS.pack name

-- | Display possibly nonexistent timestamp.
showOptionalTime :: Maybe UTCTime -> String
showOptionalTime mbTime = case mbTime of
  Just t -> show t
  _      -> "-"

-- | Reevaluate dirent size based on its direct children.
calculateSize :: DirEntry -> DirEntry
calculateSize (Right dir) = Right $ dir { dGetSize = newSize } where
  children = Map.elems $ dGetChildren dir
  getSize (Left  f) = fGetSize f
  getSize (Right d) = dGetSize d
  childSizes = map getSize children
  newSize    = sum childSizes
calculateSize file = file
