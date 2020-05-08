-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module CommandHelpers
  ( makePathAbsolute
  , getDirentryByPath
  , isPathAbsent
  , addDirEntry
  , rmDirEntry
  , getDirEntry
  , getTrackerData
  , forgetDirEntry
  , forgetDirEntryIfTracked
  , modifyDirEntryAtPath
  )
where

import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                )
import           Control.Monad.State.Lazy
import           ShellData                      ( ShellState(..)
                                                , CommandException(..)
                                                )
import           FileSystem                     ( Dir(..)
                                                , TrackerData(..)
                                                , DirEntry
                                                , emptyDir
                                                , calculateSize
                                                , getDirEntryByFullPath
                                                )
import           System.FilePath.Posix
import           PathUtils                      ( fullNormalize )
import qualified Data.Map.Strict               as Map

makePathAbsolute
  :: FilePath -> ExceptT CommandException (State ShellState) FilePath
makePathAbsolute path = do
  st <- lift get
  return $ (sGetPwd st) </> fullNormalize path

getDirentryByPath
  :: FilePath -> ExceptT CommandException (State ShellState) DirEntry
getDirentryByPath path = do
  fullPath <- makePathAbsolute path
  st       <- lift get
  case getDirEntryByFullPath (sGetRootDir st) fullPath of
    Nothing -> throwError UnknownException
    Just d  -> return d

isPathAbsent :: FilePath -> ExceptT CommandException (State ShellState) Bool
isPathAbsent path = do
  st <- lift get
  let fullPath = (sGetPwd st) </> path -- TODO: normalize?
  case getDirEntryByFullPath (sGetRootDir st) fullPath of
    Nothing -> return True
    _       -> return False

addDirEntry
  :: DirEntry -> FilePath -> ExceptT CommandException (State ShellState) ()
addDirEntry dirent path = do
  st <- lift get
  let oldDir = sGetRootDir st
  let newDir = addDirEntryAtPath oldDir dirent path
  let newSt  = st { sGetRootDir = newDir }
  put newSt

rmDirEntry :: FilePath -> ExceptT CommandException (State ShellState) ()
rmDirEntry path = do
  st <- lift get
  let oldDir = sGetRootDir st
  let newDir = rmDirEntryAtPath oldDir path
  let newSt  = st { sGetRootDir = newDir }
  put newSt

getDirEntry :: FilePath -> ExceptT CommandException (State ShellState) DirEntry
getDirEntry path = do
  fullPath <- makePathAbsolute path
  st <- lift get
  let rootDir = sGetRootDir st
  case getDirEntryByFullPath rootDir fullPath of
    Nothing -> throwError UnknownException
    Just de -> return de

getTrackerData :: FilePath -> ExceptT CommandException (State ShellState) TrackerData
getTrackerData path = do
  dirent <- getDirEntry path
  case dirent of
    Left _ -> throwError UnknownException
    Right dir -> case dGetTrackerData dir of
      Nothing -> throwError UnknownException
      Just td -> return td

forgetDirEntry :: FilePath -> ExceptT CommandException (State ShellState) ()
forgetDirEntry = undefined -- use removeFilesFromTrackerData, mb listFilesInDirEntry

forgetDirEntryIfTracked
  :: FilePath -> ExceptT CommandException (State ShellState) ()
forgetDirEntryIfTracked = undefined

type DirEntryFunction
  = Maybe DirEntry -> Either CommandException (Maybe DirEntry)

type TrackerDataFunction
  = Maybe TrackerData -> Either CommandException (Maybe TrackerData)

modifyDirEntryAtPath :: Dir -> FilePath -> DirEntryFunction -> Either CommandException (Maybe Dir)
modifyDirEntryAtPath rootDir fullPath func = do
    mbDirent <- modifyDirEntryAtPathComponents (Right rootDir) pathComponents func
    case mbDirent of
      Nothing -> return $ Just emptyDir
      Just (Left _) -> Left UnknownException
      Just (Right dir) -> return $ Just dir
  where
  normalizedPath = fullNormalize fullPath
  -- We omit "/", which is always present in split full paths
  pathComponents = tail $ splitDirectories normalizedPath
  modifyDirEntryAtPathComponents :: DirEntry -> [FilePath] -> DirEntryFunction -> Either CommandException (Maybe DirEntry)
  modifyDirEntryAtPathComponents de [] f = f (Just de)
  modifyDirEntryAtPathComponents (Left _) _ _ = Left UnknownException
  modifyDirEntryAtPathComponents (Right dir) (c:cs) f = do
    resultingMbChild <- optResultingMbChild
    let resultingChildren = case resultingMbChild of
          Nothing -> Map.delete c originalChildren
          Just resChild -> Map.insert c resChild originalChildren
    let updatedDir = dir {dGetChildren = resultingChildren}
    return $ Just $ calculateSize $ Right updatedDir
    where
    originalChildren = dGetChildren dir
    originalMbChild = Map.lookup c originalChildren
    optResultingMbChild = case originalMbChild of
      Nothing -> if null cs then f Nothing else Left UnknownException
      Just de -> modifyDirEntryAtPathComponents de cs f

modifyTrackerDataAtPath
  :: Dir -> FilePath -> (TrackerData -> TrackerData) -> Maybe Dir
modifyTrackerDataAtPath dir path f = modifyDirEntryAtPath dir path newF
  where newF de = undefined
-- case de of; TODO: how to distinguish files and dirs here? keep files as is, change dirs?
-- try to use modifyDirEntryAtPath

-- addDirEntryAtPath: use modifyDirEntryAtPath
-- rmDirEntryAtPath: use modifyDirEntryAtPath
addDirEntryAtPath :: Dir -> DirEntry -> FilePath -> Dir
addDirEntryAtPath = undefined
-- terrible note: this has to update sizes of all parent dirs

rmDirEntryAtPath :: Dir -> FilePath -> Dir
rmDirEntryAtPath = undefined
-- and this, too
-- and even worse, both of these can fail, so should mb return Maybe

-- now, consider also File -> File and Dir -> Dir; consider if they worked for
-- <nothing> -> de and de -> <nothing> cases; doesn't that describe most of your shell code?!
-- now note how DirEnt -> DirEnt should also be recalculating file and dir sizes
-- and now consider f :: Maybe <f/d/de> ->; would that help?
