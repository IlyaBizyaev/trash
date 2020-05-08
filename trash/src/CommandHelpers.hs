-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module CommandHelpers
  ( makePathAbsolute
  , addDirEntry
  , replaceDirEntry
  , rmDirEntry
  , getDirEntry
  , getTrackerData
  , forgetDirEntry
  , forgetDirEntryIfTracked
  , modifyDirEntryAtPath
  , modifyTrackerDataAtPath
  , modifyTrackerData
  , getTrackerDirectory
  , updateTrackerPath
  , DirEntryFunction
  , TrackerDataFunction
  )
where

import Control.Monad.Except (ExceptT, catchError, throwError)
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map
import FileSystem (Dir (..), DirEntry, TrackerData (..), calculateSize, emptyDir,
                   getDirEntryByFullPath, isDirTracked, listFilesInDirEntry,
                   removeFilesFromTrackerData)
import PathUtils (fullNormalize, makeRelativeTo)
import ShellData (CommandException (..), ShellState (..))
import System.FilePath.Posix

makePathAbsolute
  :: FilePath -> ExceptT CommandException (State ShellState) FilePath
makePathAbsolute path = do
  st <- lift get
  return $ (sGetPwd st) </> fullNormalize path

getTrackerDirectory :: ExceptT CommandException (State ShellState) FilePath
getTrackerDirectory = do
  st <- lift get
  case sGetTrackerDir st of
    Nothing   -> throwError LocationNotTracked
    Just path -> return path

updateTrackerPath :: ShellState -> ShellState
updateTrackerPath st = st { sGetTrackerDir = newTrackerDir } where
  rootDir          = sGetRootDir st
  newPwd           = sGetPwd st
  newPwdComponents = (reverse . splitDirectories) newPwd
  newTrackerDir    = findClosestTrackedAncestor newPwdComponents
  findClosestTrackedAncestor []         = Nothing
  findClosestTrackedAncestor x@(_ : xs) = if isSubdirAtPathTracked curPath
    then Just curPath
    else findClosestTrackedAncestor xs
    where curPath = (joinPath . reverse) x
  isSubdirAtPathTracked p = case getDirEntryByFullPath rootDir p of
    Just (Right dir) -> isDirTracked dir
    _                -> False

addDirEntry
  :: DirEntry -> FilePath -> ExceptT CommandException (State ShellState) ()
addDirEntry dirent path = do
  st <- lift get
  let oldDir    = sGetRootDir st
  let optNewDir = addDirEntryAtPath oldDir dirent path
  case optNewDir of
    Left  e   -> throwError e
    Right dir -> put st { sGetRootDir = dir }

replaceDirEntry
  :: DirEntry -> FilePath -> ExceptT CommandException (State ShellState) ()
replaceDirEntry dirent path = do
  st <- lift get
  let oldDir    = sGetRootDir st
  let optNewDir = replaceDirEntryAtPath oldDir dirent path
  case optNewDir of
    Left  e   -> throwError e
    Right dir -> put st { sGetRootDir = dir }

rmDirEntry :: FilePath -> ExceptT CommandException (State ShellState) ()
rmDirEntry path = do
  st <- lift get
  let oldDir    = sGetRootDir st
  let optNewDir = rmDirEntryAtPath oldDir path
  case optNewDir of
    Left  e   -> throwError e
    Right dir -> put st { sGetRootDir = dir }

getDirEntry :: FilePath -> ExceptT CommandException (State ShellState) DirEntry
getDirEntry path = do
  fullPath <- makePathAbsolute path
  st       <- lift get
  let rootDir = sGetRootDir st
  case getDirEntryByFullPath rootDir fullPath of
    Nothing -> throwError ObjectNotFound
    Just de -> return de

getTrackerData :: ExceptT CommandException (State ShellState) TrackerData
getTrackerData = do
  trackerDir <- getTrackerDirectory
  dirent     <- getDirEntry trackerDir
  case dirent of
    Left  _   -> throwError IllegalObjectType
    Right dir -> case dGetTrackerData dir of
      Nothing -> throwError LocationNotTracked
      Just td -> return td

modifyTrackerData
  :: TrackerDataFunction -> ExceptT CommandException (State ShellState) ()
modifyTrackerData f = do
  st <- lift get
  let rootDir       = sGetRootDir st
  let mbTrackerPath = sGetTrackerDir st
  case mbTrackerPath of
    Nothing -> throwError LocationNotTracked
    Just td -> do
      let fResult = modifyTrackerDataAtPath rootDir td f
      case fResult of
        Left  e   -> throwError e
        Right dir -> do
          let newSt = st { sGetRootDir = dir }
          put newSt

forgetDirEntry :: FilePath -> ExceptT CommandException (State ShellState) ()
forgetDirEntry path = do
  fullPath    <- makePathAbsolute path
  dirent      <- getDirEntry path
  trackerPath <- getTrackerDirectory
  let relativePath = makeRelativeTo trackerPath fullPath
  case relativePath of
    Nothing      -> throwError ObjectIsNotAChild
    Just relPath -> do
      let pathsToForget = listFilesInDirEntry relPath dirent
      modifyTrackerData (f pathsToForget)
 where
  f :: [FilePath] -> TrackerDataFunction
  f _ Nothing = Left LocationNotTracked
  f paths (Just trackerData) =
    case removeFilesFromTrackerData trackerData paths of
      Nothing -> Left FileIsNotInTrackerData
      Just td -> return $ Just td

forgetDirEntryIfTracked
  :: FilePath -> ExceptT CommandException (State ShellState) ()
forgetDirEntryIfTracked path =
  catchError (forgetDirEntry path) (\_ -> return ())

type DirEntryFunction
  = Maybe DirEntry -> Either CommandException (Maybe DirEntry)

type TrackerDataFunction
  = Maybe TrackerData -> Either CommandException (Maybe TrackerData)

modifyDirEntryAtPath
  :: Dir -> FilePath -> DirEntryFunction -> Either CommandException (Maybe Dir)
modifyDirEntryAtPath rootDir fullPath func = do
  mbDirent <- modifyDirEntryAtPathComponents (Right rootDir) pathComponents func
  case mbDirent of
    Nothing          -> return $ Just emptyDir
    Just (Left  _  ) -> Left ShellInternalError
    Just (Right dir) -> return $ Just dir
 where
  normalizedPath = fullNormalize fullPath
  -- We omit "/", which is always present in split full paths
  pathComponents = tail $ splitDirectories normalizedPath
  modifyDirEntryAtPathComponents
    :: DirEntry
    -> [FilePath]
    -> DirEntryFunction
    -> Either CommandException (Maybe DirEntry)
  modifyDirEntryAtPathComponents de          []       f = f (Just de)
  modifyDirEntryAtPathComponents (Left  _  ) _        _ = Left ObjectNotFound
  modifyDirEntryAtPathComponents (Right dir) (c : cs) f = do
    resultingMbChild <- optResultingMbChild
    let resultingChildren = case resultingMbChild of
          Nothing       -> Map.delete c originalChildren
          Just resChild -> Map.insert c resChild originalChildren
    let updatedDir = dir { dGetChildren = resultingChildren }
    return $ Just $ calculateSize $ Right updatedDir
   where
    originalChildren    = dGetChildren dir
    originalMbChild     = Map.lookup c originalChildren
    optResultingMbChild = case originalMbChild of
      Nothing -> if null cs then f Nothing else Left ObjectNotFound
      Just de -> modifyDirEntryAtPathComponents de cs f

modifyTrackerDataAtPath
  :: Dir -> FilePath -> TrackerDataFunction -> Either CommandException Dir
modifyTrackerDataAtPath dir path f = do
  direntModification <- modifyDirEntryAtPath dir path newF
  case direntModification of
    Nothing -> Left ShellInternalError
    Just d  -> return d
 where
  newF :: Maybe DirEntry -> Either CommandException (Maybe DirEntry)
  newF Nothing          = Left ObjectNotFound
  newF (Just (Left  _)) = Left IllegalObjectType
  newF (Just (Right d)) = do
    let trackerData = dGetTrackerData d
    fResult <- f trackerData
    return $ Just $ Right d { dGetTrackerData = fResult }

addDirEntryAtPath :: Dir -> DirEntry -> FilePath -> Either CommandException Dir
addDirEntryAtPath rootDir dirEntry path = do
  call <- modifyDirEntryAtPath rootDir path f
  case call of
    Nothing  -> Left ShellInternalError
    Just dir -> return dir
 where
  f :: DirEntryFunction
  f Nothing = Right $ Just $ dirEntry
  f _       = Left ObjectAlreadyExists

replaceDirEntryAtPath
  :: Dir -> DirEntry -> FilePath -> Either CommandException Dir
replaceDirEntryAtPath rootDir dirEntry path = do
  call <- modifyDirEntryAtPath rootDir path f
  case call of
    Nothing  -> Left ShellInternalError
    Just dir -> return dir
 where
  f :: DirEntryFunction
  f _ = Right $ Just $ dirEntry

rmDirEntryAtPath :: Dir -> FilePath -> Either CommandException Dir
rmDirEntryAtPath rootDir path = do
  call <- modifyDirEntryAtPath rootDir path f
  case call of
    Nothing  -> return emptyDir
    Just dir -> return dir
 where
  f :: DirEntryFunction
  f Nothing = Left ObjectNotFound
  f _       = Right Nothing

