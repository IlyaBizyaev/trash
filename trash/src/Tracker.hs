-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module Tracker
  ( initCmd
  , addCmd
  , logCmd
  , forgetCmd
  , forgetRevCmd
  , checkoutCmd
  , mergeCmd
  )
where

import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                )
import           Control.Monad.State.Lazy
import           ShellData                      ( ShellState(..)
                                                , CommandException(..)
                                                )
import           FileSystem                     ( isDirTracked )
import           CommandHelpers                 ( getDirentryByPath, forgetDirEntry )

initCmd :: ExceptT CommandException (State ShellState) String
initCmd = do
  dirent <- getDirentryByPath "."
  case dirent of
    Left  _   -> throwError UnknownException
    Right dir -> if isDirTracked dir
      then throwError UnknownException
      else do
        return ""
-- learn to put emptyTrackerData deep into the structure, and how to generalize it for other funcs...
-- at least the action of VCS replacement

addCmd :: FilePath -> ExceptT CommandException (State ShellState) String
addCmd path = undefined
-- use: addRevisionsToTrackerData, modifyTrackerData
-- ensure vcs dir is not Nothing
-- get full path of the target path
-- verify full path is a child/eq of tracker path
-- get that path relative to tracker path
-- get dirent for path (normalized relative? or works with absolute?)
-- for file: get contents
-- for dir: get all files in that dir, get their contents
-- build map vscRelPath -> currentContent for all files affected
-- function on vcs dir: add new revisions for affected files (external f, increments rev cnt)
-- TODO: later, also check if anything has been modified before doing that


logCmd :: FilePath -> ExceptT CommandException (State ShellState) String
logCmd path = undefined
-- use: getLogFromTrackerData, listFilesInDirEntry
-- ensure vcs dir is not Nothing
-- get full path of the target path
-- verify full path is a child/eq of tracker path
-- get that path relative to tracker path
-- get dirent for path (normalized relative? or works with absolute?)
-- get vcs dir
-- get tracker data
-- for file dirent: query the rel path in tracker data
-- for dir dirent: get all paths of children recursively (generalize query f), query all their revisions
-- sort revisions, merge files with same rev number (result of dir add)
-- pretty-print

forgetCmd :: FilePath -> ExceptT CommandException (State ShellState) String
forgetCmd path = do
  forgetDirEntry path
  return ""
-- function on state's tracker dir data:
-- * if no tracker dir, fail with error
-- * if have tracker dir but not file's revision log in it, fail with error
-- * otherwise delete file's key from tracker dir data
-- along the way, need file path relative to tracker dir (child/eq, or fail)

forgetRevCmd
  :: FilePath -> Integer -> ExceptT CommandException (State ShellState) String
forgetRevCmd path rev = undefined
-- use: removeRevisionFromTrackerData, modifyTrackerData
-- function on state's tracker dir data:
-- * if no tracker dir, fail with error
-- * if have tracker dir but not file's revision log in it, fail with error
-- * if have file revision log but not this rev in it, fail with error
-- * otherwise delete file's key from tracker dir data
-- along the way, need file path relative to tracker dir (child/eq, or fail)

checkoutCmd
  :: FilePath -> Integer -> ExceptT CommandException (State ShellState) String
checkoutCmd path rev = undefined
-- use: getRevisionFromTrackerData
-- ensure vcs dir is not Nothing
-- get full path of the target path
-- verify full path is a child/eq of tracker path
-- get that path relative to tracker path
-- get dirent for path (normalized relative? or works with absolute?)
-- get vcs dir
-- get tracker data
-- for file dirent: query specified rev, display contents/fail
-- for dir dirent: display error message

mergeCmd
  :: FilePath
  -> Integer
  -> Integer
  -> String
  -> ExceptT CommandException (State ShellState) String
mergeCmd path rev1 rev2 strategy = undefined
-- use: getRevisionFromTrackerData
-- ensure vcs dir is not Nothing
-- get full path of the target path
-- verify full path is a child/eq of tracker path
-- get that path relative to tracker path
-- get dirent for path (normalized relative? or works with absolute?)
-- get vcs dir
-- get tracker data
-- for file dirent: query specified revs, fail if one is not present, choose one based on strategy, write to file
-- for dir dirent: display error message
