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
import           PathUtils                      ( isChildOfPath
                                                , makeRelativeTo
                                                )
import           FileSystem                     ( File(..)
                                                , FileRevision(..)
                                                , isDirTracked
                                                , emptyTrackerData
                                                , addRevisionsToTrackerData
                                                , listFilesInDirEntry
                                                , getLogFromTrackerData
                                                )
import           CommandHelpers                 ( makePathAbsolute
                                                , getDirEntry
                                                , forgetDirEntry
                                                , modifyTrackerData
                                                , getTrackerDirectory
                                                , getTrackerData
                                                , TrackerDataFunction
                                                )
import           Data.List                      ( intercalate )

initCmd :: ExceptT CommandException (State ShellState) String
initCmd = do
  modifyTrackerData f
  return ""
 where
  f :: TrackerDataFunction
  f Nothing = Right $ Just emptyTrackerData
  f _       = Left UnknownException

addCmd
  :: FilePath -> String -> ExceptT CommandException (State ShellState) String
addCmd path summary = do
  fullPath    <- makePathAbsolute path
  trackerPath <- getTrackerDirectory
  unless (isChildOfPath trackerPath fullPath) (throwError UnknownException)
  dirent <- getDirEntry path
  let filesAffected = listFilesInDirEntry fullPath dirent
  fileContents <- mapM getFileContentByPath filesAffected
  let relativePaths = mapM (makeRelativeTo trackerPath) filesAffected
  case relativePaths of
    Nothing       -> throwError UnknownException
    Just relPaths -> do
      let newRevs = zipWith (prepareFileRev summary) relPaths fileContents
      modifyTrackerData (f newRevs)
      return ""
 where
  prepareFileRev summ p content = (p, FileRevision summ content)
  getFileContentByPath p = do
    de <- getDirEntry p
    case de of
      Right _    -> throwError UnknownException
      Left  file -> return $ fGetContent file
  f :: [(FilePath, FileRevision)] -> TrackerDataFunction
  f _    Nothing   = Left UnknownException
  f revs (Just td) = Right $ Just $ addRevisionsToTrackerData td revs

logCmd :: FilePath -> ExceptT CommandException (State ShellState) String
logCmd path = do
  fullPath    <- makePathAbsolute path
  dirent <- getDirEntry path
  trackerPath <- getTrackerDirectory
  let filePaths = listFilesInDirEntry fullPath dirent
  trackerData <- getTrackerData
  let relativePaths = mapM (makeRelativeTo trackerPath) filePaths
  case relativePaths of
    Nothing -> throwError UnknownException
    Just relPaths -> do
      let revisionLog = getLogFromTrackerData trackerData relPaths
      case revisionLog of
        Nothing -> throwError UnknownException
        Just revLog -> do
          let output = concatMap (\(ver, rs) -> ("# Revision " ++ show ver):(map show rs)) revLog
          return $ intercalate "\n" output

forgetCmd :: FilePath -> ExceptT CommandException (State ShellState) String
forgetCmd path = do
  forgetDirEntry path
  return ""

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
