-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module RealIO
  ( readDirEntryFromFilesystem
  , writeDirToFilesystem
  )
where

import           FileSystem                     ( Dir(..)
                                                , File(..)
                                                , DirEntry
                                                , TrackerData(..)
                                                , FileRevision(..)
                                                , calculateSize
                                                )
import           System.Directory               ( doesPathExist
                                                , doesDirectoryExist
                                                , pathIsSymbolicLink
                                                , getPermissions
                                                , getModificationTime
                                                , listDirectory
                                                , getFileSize
                                                , doesFileExist
                                                )
import           System.FilePath.Posix
import           Control.Exception              ( throwIO )
import           Control.Monad                  ( when )
import qualified Data.Map.Strict               as Map
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.List                      ( elemIndex )
import           PathUtils                      ( lastSegment )

trackerSubdirName = ".tracker"
indexFileName = "index"

readDirEntryFromFilesystem :: FilePath -> IO DirEntry
readDirEntryFromFilesystem objectPath = do
  _         <- doesPathExist objectPath
  isSymLink <- pathIsSymbolicLink objectPath
  when isSymLink (throwIO $ userError "Symlinks are not supported")
  isDir <- doesDirectoryExist objectPath
  if isDir
    then do
      dir <- readDirFromFS objectPath
      return $ Right dir
    else do
      file <- readFileFromFS objectPath
      return $ Left file where
  readDirFromFS path = do
    permissions <- getPermissions path
    trackerData <- readDirTrackerData path
    childPaths  <- listDirectory path
    let childPathsWithoutTracker = filter (/= trackerSubdirName) childPaths
    let childFullPaths           = map (path </>) childPathsWithoutTracker
    children <- mapM readDirEntryFromFilesystem childFullPaths
    let childrenList = zip childPathsWithoutTracker children
    let childrenMap  = Map.fromList childrenList
    let zeroSizeDirEnt = Right $ Dir { dGetTrackerData = trackerData
                                     , dGetPermissions = permissions
                                     , dGetSize        = 0
                                     , dGetChildren    = childrenMap
                                     }
    let sizedDirEnt = calculateSize zeroSizeDirEnt
    case sizedDirEnt of
      Left  _   -> throwIO $ userError "unreachable"
      Right dir -> return dir
  readFileFromFS path = do
    permissions <- getPermissions path
    modTime     <- getModificationTime path
    content     <- B.readFile path
    size        <- getFileSize path
    return $ File { fGetPermissions      = permissions
                  , fGetModificationTime = Just modTime
                  , fGetSize             = size
                  , fGetContent          = content
                  }
  listDirectoryRecursively dirPath = (return [] :: IO [FilePath])
  splitLast char str =
    let n = elemIndex char (reverse str)
    in  case n of
          Nothing -> (str, [])
          Just x  -> splitAt (length str - x - 1) str
  safeReadInteger :: String -> Maybe Integer
  safeReadInteger s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing
  relPathToRev path = parseRev . (splitLast '_') $ path where
    parseRev (p, shouldBeVer) = do
      ver <- safeReadInteger shouldBeVer
      return (p, ver)
  insertRevToMap oldMap ((path, ver), content) = case Map.lookup path oldMap of
    Nothing -> Map.insert path (Map.singleton ver revision) oldMap
    Just mapForPath -> Map.insert path (Map.insert ver revision mapForPath) oldMap
    where
      revision = FileRevision {frGetName = "keklolidk", frGetContent = content}
  readIndexFile indexContent = undefined
  readDirTrackerData path = do
    let trackerSubdirPath = path </> trackerSubdirName
    trackerSubdirExists <- doesDirectoryExist trackerSubdirPath
    let indexFilePath = trackerSubdirPath </> indexFileName
    indexFileExists <- doesFileExist indexFilePath
    if (not trackerSubdirExists || not indexFileExists)
      then return Nothing
      else do
        indexContent <- B.readFile indexFilePath
        let maybeLastVersion = BC.readInteger indexContent
        case maybeLastVersion of
          Nothing -> throwIO $ userError "Invalid index file in tracker data"
          Just (lastVersion, _) -> do
            childPaths <- listDirectoryRecursively trackerSubdirPath
            let childPathsWithoutIndex = filter (/= indexFilePath) childPaths
            let childFullPaths = map (path </>) childPathsWithoutIndex
            contents <- mapM B.readFile childFullPaths
            let childPathsWithRevisionIndexes =
                  mapM relPathToRev childPathsWithoutIndex
            case childPathsWithRevisionIndexes of
              Nothing -> throwIO $ userError "Invalid revision files in tracker data"
              Just revPairs -> do
                let z = zip revPairs contents
                let newRevs = foldl insertRevToMap Map.empty z
                return $ Just $ TrackerData {tGetLastVersion = lastVersion, tGetRevisions = newRevs}


writeDirToFilesystem :: Dir -> IO ()
writeDirToFilesystem dir = undefined
