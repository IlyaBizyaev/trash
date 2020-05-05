-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module CommandHelpers
  ( getDirentryByPath
  , isPathAbsent
  , addDirEntryToState
  , rmDirEntryFromState
  , isFileTracked
  , forgetDirEntry
  )
where

import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                )
import           Control.Monad.State.Lazy
import           ShellData                      ( ShellState(..)
                                                , CommandException(..)
                                                )
import           FileSystem                     ( DirEntry
                                                , getDirentryByFullPath
                                                , addDirEntry
                                                , rmDirEntry
                                                , isFileTrackedInDir
                                                )
import           System.FilePath.Posix

getDirentryByPath
  :: FilePath -> ExceptT CommandException (State ShellState) DirEntry
getDirentryByPath path = do
  st <- lift get
  let fullPath = (sGetPwd st) </> path -- TODO: normalize?
  case getDirentryByFullPath (sGetRootDir st) fullPath of
    Nothing -> throwError UnknownException
    Just d  -> return d

isPathAbsent :: FilePath -> ExceptT CommandException (State ShellState) Bool
isPathAbsent path = do
  st <- lift get
  let fullPath = (sGetPwd st) </> path -- TODO: normalize?
  case getDirentryByFullPath (sGetRootDir st) fullPath of
    Nothing -> return True
    _       -> return False

addDirEntryToState
  :: DirEntry -> FilePath -> ExceptT CommandException (State ShellState) ()
addDirEntryToState dirent path = do
  st <- lift get
  let oldDir = sGetRootDir st
  let newDir = addDirEntry oldDir dirent path
  let newSt  = st { sGetRootDir = newDir }
  put newSt

rmDirEntryFromState
  :: FilePath -> ExceptT CommandException (State ShellState) ()
rmDirEntryFromState path = do
  st <- lift get
  let oldDir = sGetRootDir st
  let newDir = rmDirEntry oldDir path
  let newSt  = st { sGetRootDir = newDir }
  put newSt

isFileTracked :: FilePath -> ExceptT CommandException (State ShellState) Bool
isFileTracked path = do
  st <- lift get
  let trackerDir = sGetTrackerDir st
  case trackerDir of
    Nothing   -> throwError UnknownException
    Just tDir -> do
      trackerDirent <- getDirentryByPath tDir
      case trackerDirent of
        Left  _   -> throwError UnknownException
        Right dir -> return $ isFileTrackedInDir dir path

forgetDirEntry :: FilePath -> ExceptT CommandException (State ShellState) ()
forgetDirEntry = undefined
