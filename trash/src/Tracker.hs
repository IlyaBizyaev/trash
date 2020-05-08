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
                                                , emptyTrackerData
                                                , addRevisionsToTrackerData
                                                , listFilesInDirEntry
                                                , getLogFromTrackerData
                                                , removeRevisionFromTrackerData
                                                , getRevisionFromTrackerData
                                                )
import           CommandHelpers                 ( makePathAbsolute
                                                , getDirEntry
                                                , forgetDirEntry
                                                , replaceDirEntry
                                                , modifyTrackerData
                                                , getTrackerDirectory
                                                , getTrackerData
                                                , TrackerDataFunction
                                                )
import           Data.List                      ( intercalate )
import           Data.Either                    ( isRight )
import qualified Data.ByteString               as B

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
  dirent      <- getDirEntry path
  trackerPath <- getTrackerDirectory
  let filePaths = listFilesInDirEntry fullPath dirent
  trackerData <- getTrackerData
  let relativePaths = mapM (makeRelativeTo trackerPath) filePaths
  case relativePaths of
    Nothing       -> throwError UnknownException
    Just relPaths -> do
      let revisionLog = getLogFromTrackerData trackerData relPaths
      case revisionLog of
        Nothing     -> throwError UnknownException
        Just revLog -> do
          let output = concatMap
                (\(ver, rs) -> ("# Revision " ++ show ver) : (map show rs))
                revLog
          return $ intercalate "\n" output

forgetCmd :: FilePath -> ExceptT CommandException (State ShellState) String
forgetCmd path = do
  forgetDirEntry path
  return ""

forgetRevCmd
  :: FilePath -> Integer -> ExceptT CommandException (State ShellState) String
forgetRevCmd path rev = do
  fullPath    <- makePathAbsolute path
  trackerPath <- getTrackerDirectory
  let relativePath = makeRelativeTo trackerPath fullPath
  case relativePath of
    Nothing      -> throwError UnknownException
    Just relPath -> do
      modifyTrackerData (f relPath)
      return ""
 where
  f :: FilePath -> TrackerDataFunction
  f _ Nothing = Left UnknownException
  f p (Just trackerData) =
    case removeRevisionFromTrackerData trackerData p rev of
      Nothing -> Left UnknownException
      Just td -> return $ Just td

checkoutCmd
  :: FilePath -> Integer -> ExceptT CommandException (State ShellState) String
checkoutCmd path version = do
  trackerData <- getTrackerData
  fullPath    <- makePathAbsolute path
  dirent      <- getDirEntry path
  when (isRight dirent) (throwError UnknownException)
  trackerPath <- getTrackerDirectory
  let relativePath = makeRelativeTo trackerPath fullPath
  case relativePath of
    Nothing      -> throwError UnknownException
    Just relPath -> do
      let mbRev = getRevisionFromTrackerData trackerData relPath version
      case mbRev of
        Nothing  -> throwError UnknownException
        Just rev -> return $ show rev

mergeCmd
  :: FilePath
  -> Integer
  -> Integer
  -> String
  -> ExceptT CommandException (State ShellState) String
mergeCmd path ver1 ver2 strategy = do
  fullPath <- makePathAbsolute path
  dirent   <- getDirEntry path
  case dirent of
    Right _    -> throwError UnknownException
    Left  file -> do
      trackerPath <- getTrackerDirectory
      trackerData <- getTrackerData
      let relativePath = makeRelativeTo trackerPath fullPath
      case relativePath of
        Nothing      -> throwError UnknownException
        Just relPath -> do
          let
            revisions =
              mapM (getRevisionFromTrackerData trackerData relPath) [ver1, ver2]
          case revisions of
            Just [firstRev, secondRev] -> do
              newContent <- mergeWithStrategy firstRev secondRev strategy
              let newDirent = Left $ file { fGetContent = newContent }
              replaceDirEntry newDirent fullPath
              return ""
            _ -> throwError UnknownException
 where
  mergeWithStrategy
    :: FileRevision
    -> FileRevision
    -> String
    -> ExceptT CommandException (State ShellState) B.ByteString
  mergeWithStrategy rev1 rev2 s = case s of
    "left"  -> return $ frGetContent rev1
    "right" -> return $ frGetContent rev2
    "both"  -> return (B.append (frGetContent rev1) (frGetContent rev2))
    _       -> throwError UnknownException


