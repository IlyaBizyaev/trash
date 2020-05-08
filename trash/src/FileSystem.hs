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
  , isFileTrackedInDir
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
  )
where

import           System.Directory               ( Permissions
                                                , emptyPermissions
                                                )
import           Data.Time.Clock                ( UTCTime )
import qualified Data.Map.Strict               as Map
import           System.FilePath.Posix
import           PathUtils                      ( fullNormalize )
import           Network.Mime
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.List                      ( isInfixOf )
import qualified Data.Text                     as TS

data FileRevision = FileRevision {
  frGetName :: String,
  frGetContent :: B.ByteString
}

instance Show FileRevision where
  show revision = summaryLine ++ '\n':contentBlock where
    summaryLine = "Summary: " ++ frGetName revision
    contentBlock = BC.unpack $ frGetContent revision

-- TODO: how to reliably set permissions for the file that we are going to create? Do we know them in advance?
data File = File {
  fGetPermissions :: Permissions,
  fGetModificationTime :: Maybe UTCTime,
  fGetSize :: Integer,
  fGetContent :: B.ByteString
}

data TrackerData = TrackerData {
  tGetLastVersion :: Integer,
  tGetRevisions :: Map.Map String (Map.Map Integer FileRevision)
}

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
  dGetSize :: Integer,
  dGetChildren :: Map.Map FilePath DirEntry
}

emptyDir :: Dir
emptyDir = Dir { dGetTrackerData = Nothing
               , dGetPermissions = emptyPermissions
               , dGetSize        = 0
               , dGetChildren    = Map.empty
               }

emptyTrackerData :: TrackerData
emptyTrackerData =
  TrackerData { tGetLastVersion = 0, tGetRevisions = Map.empty }

type DirEntry = Either File Dir

-- TODO: learn to properly pack the content, maybe for another input format (Text?)
-- TODO: and verify that size measuring makes sense
buildFileWithContent :: String -> File
buildFileWithContent content = File { fGetPermissions      = defaultPermissions
                                    , fGetModificationTime = Nothing
                                    , fGetSize             = byteStringSize
                                    , fGetContent          = byteStringContent
                                    }
 where
  byteStringContent  = BC.pack content
  byteStringSize     = (toInteger . B.length) byteStringContent
  defaultPermissions = emptyPermissions

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

calculateSize :: DirEntry -> DirEntry
calculateSize (Right dir) = Right $ dir { dGetSize = newSize } where
  children = Map.elems $ dGetChildren dir
  getSize (Left  f) = fGetSize f
  getSize (Right d) = dGetSize d
  childSizes = map getSize children
  newSize    = sum childSizes
calculateSize file = file
