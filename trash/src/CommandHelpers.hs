-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module CommandHelpers
  ( makePathAbsolute
  , getDirentryByPath
  , isPathAbsent
  , addDirEntry
  , rmDirEntry
  , forgetDirEntry
  , forgetDirEntryIfTracked
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
                                                , getDirEntryByFullPath
                                                , addDirEntryAtPath
                                                , rmDirEntryAtPath
                                                , isFileTrackedInDir
                                                )
import           System.FilePath.Posix
import           PathUtils                      ( fullNormalize )

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

forgetDirEntry :: FilePath -> ExceptT CommandException (State ShellState) ()
forgetDirEntry = undefined

forgetDirEntryIfTracked
  :: FilePath -> ExceptT CommandException (State ShellState) ()
forgetDirEntryIfTracked = undefined

type DirEntryFunction
  = Maybe DirEntry -> Either CommandException (Maybe DirEntry)

type TrackerDataFunction
  = Maybe TrackerData -> Either CommandException (Maybe TrackerData)

-- the complication here is splitting the path, while considering that head can be '/'
-- maybe we can call getDirEntryByFullPath, which should be smart enough to implement this concept?
modifyDirEntryAtPath :: Dir -> FilePath -> DirEntryFunction -> Maybe Dir
modifyDirEntryAtPath dir path f = undefined

modifyTrackerDataAtPath
  :: Dir -> FilePath -> (TrackerData -> TrackerData) -> Maybe Dir
modifyTrackerDataAtPath dir path f = modifyDirEntryAtPath dir path newF
  where newF de = undefined -- case de of; TODO: how to distinguish files and dirs here? keep files as is, change dirs?


-- now, consider also File -> File and Dir -> Dir; consider if they worked for
-- <nothing> -> de and de -> <nothing> cases; doesn't that describe most of your shell code?!
-- now note how DirEnt -> DirEnt should also be recalculating file and dir sizes
-- and now consider f :: Maybe <f/d/de> ->; would that help?
