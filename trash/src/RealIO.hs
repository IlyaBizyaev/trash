-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module RealIO
  ( readDirEntryFromFilesystem
  , writeDirEntryToFilesystem
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
                                                , setModificationTime
                                                , setPermissions
                                                , createDirectoryIfMissing
                                                , removePathForcibly
                                                , createDirectory
                                                )
import           System.FilePath.Posix
import           Control.Exception              ( throwIO )
import           Control.Monad                  ( when )
import qualified Data.Map.Strict               as Map
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.List                      ( elemIndex
                                                , partition
                                                , (\\)
                                                , intercalate
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Foldable                  ( mapM_ )

trackerSubdirName :: String
trackerSubdirName = ".tracker"

indexFileName :: String
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
  listDirectoryRecursively dirPath = do
    children <- listDirectory dirPath
    let childrenFullPaths = map (dirPath </>) children
    isDir <- mapM doesDirectoryExist childrenFullPaths
    let childrenWithStatus = zip isDir childrenFullPaths
    let (subdirsWithStatus, filesWithStatus) = partition fst childrenWithStatus
    let files              = map snd filesWithStatus
    let subdirs            = map snd subdirsWithStatus
    recursiveRes <- mapM listDirectoryRecursively subdirs
    return $ files ++ concat recursiveRes
  splitLast char str =
    let n = elemIndex char (reverse str)
    in  case n of
          Nothing -> (str, [])
          Just x  -> splitAt (length str - x - 1) str
  safeReadInteger :: String -> Maybe Integer
  safeReadInteger s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing
  relPathToRev path = parseRev . (splitLast '_') $ path   where
    parseRev (p, shouldBeVer) = do
      ver <- safeReadInteger shouldBeVer
      return (p, ver)
  insertRevToMap mapVerToSumm oldMap ((path, ver), content) =
    case Map.lookup path oldMap of
      Nothing -> Map.insert path (Map.singleton ver revision) oldMap
      Just mapForPath ->
        Map.insert path (Map.insert ver revision mapForPath) oldMap
   where
    revName  = Data.Maybe.fromMaybe "untitled" (Map.lookup ver mapVerToSumm)
    revision = FileRevision { frGetName = revName, frGetContent = content }
  splitAtFirst x = fmap (drop 1) . break (x ==)
  summLineToPair line = parseRev . (splitAtFirst ' ') $ line   where
    parseRev (shouldBeVer, s) = do
      ver <- safeReadInteger shouldBeVer
      return (ver, s)
  readIndexFile indexContent = do
    let strContent   = BC.unpack indexContent
    let parseLastVer = reads strContent :: [(Integer, String)]
    case parseLastVer of
      [] -> throwIO $ userError "No last revision version in tracker index"
      [(lastVer, summaryList)] -> do
        let summaryLines      = lines summaryList
        let mbSummaryStrPairs = mapM summLineToPair summaryLines
        case mbSummaryStrPairs of
          Nothing ->
            throwIO $ userError "Illegal revision version in tracker index"
          Just summaryPairs -> return (lastVer, Map.fromList summaryPairs)
      _ -> do
        throwIO $ userError "unreachable"
  readDirTrackerData path = do
    let trackerSubdirPath = path </> trackerSubdirName
    trackerSubdirExists <- doesDirectoryExist trackerSubdirPath
    let indexFilePath = trackerSubdirPath </> indexFileName
    indexFileExists <- doesFileExist indexFilePath
    if (not trackerSubdirExists || not indexFileExists)
      then return Nothing
      else do
        indexContent <- B.readFile indexFilePath
        (lastVersion, mapVerToSummary) <- readIndexFile indexContent
        childPaths <- listDirectoryRecursively trackerSubdirPath
        let childPathsWithoutIndex = filter (/= indexFilePath) childPaths
        let childRelPaths =
              map (makeRelative trackerSubdirPath) childPathsWithoutIndex
        contents <- mapM B.readFile childPathsWithoutIndex
        let childPathsWithRevisionIndexes = mapM relPathToRev childRelPaths
        case childPathsWithRevisionIndexes of
          Nothing ->
            throwIO $ userError "Invalid revision files in tracker data"
          Just revPairs -> do
            let z       = zip revPairs contents
            let newRevs = foldl (insertRevToMap mapVerToSummary) Map.empty z
            return $ Just $ TrackerData { tGetLastVersion = lastVersion
                                        , tGetRevisions   = newRevs
                                        }


writeDirEntryToFilesystem :: FilePath -> DirEntry -> IO ()
writeDirEntryToFilesystem path (Left file) = do
  B.writeFile path (fGetContent file)
  setPermissions path (fGetPermissions file)
  let mbModTime = fGetModificationTime file
  mapM_ (setModificationTime path) mbModTime
writeDirEntryToFilesystem path (Right dir) = do
  createDirectoryIfMissing False path
  setPermissions path (dGetPermissions dir)
  curChildren <- listDirectory path
  let targetChildrenMap = dGetChildren dir
  let targetChildren    = Map.keys targetChildrenMap
  let curToDelete       = curChildren \\ targetChildren
  let targetToCreate    = targetChildren \\ curChildren
  mapM_ (removePathForcibly . (path </>)) curToDelete
  mapM_
    ( (\p -> writeDirEntryToFilesystem p (targetChildrenMap Map.! p))
    . (path </>)
    )
    targetToCreate
  case dGetTrackerData dir of
    Nothing          -> return ()
    Just trackerData -> do
      let lastVersion   = tGetLastVersion trackerData
      let revisions     = tGetRevisions trackerData
      let revValues     = Map.elems revisions
      let revVerMap     = Map.unions revValues
      let revSummaryMap = Map.map (\fr -> frGetName fr) revVerMap
      let summaryList = intercalate "\n" $ map
            (\(ver, summ) -> show ver ++ ' ' : summ)
            (Map.toList revSummaryMap)
      let indexContent = show lastVersion ++ '\n':summaryList
      let trackerSubdirPath = path </> trackerSubdirName
      removePathForcibly trackerSubdirPath
      createDirectory trackerSubdirPath
      let indexFilePath = trackerSubdirPath </> indexFileName
      B.writeFile indexFilePath (BC.pack indexContent)
      let revList = Map.toList revisions
      let revPairToRevList (p, m) = (p, Map.toList m)
      let revListToTriples (p, l) = zip (repeat p) l
      let tripleToFilename (p, (v, rev)) = ((trackerSubdirPath </> p) ++ '_':(show v), frGetContent rev)
      let revPairToFilename = map tripleToFilename . revListToTriples . revPairToRevList
      let revFileList = concatMap revPairToFilename revList
      mapM_ (uncurry B.writeFile) revFileList
