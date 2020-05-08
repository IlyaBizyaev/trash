-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module FileManager
  ( lsCmd
  , touchCmd
  , mkdirCmd
  , catCmd
  , rmCmd
  , writeCmd
  , findCmd
  , statCmd
  )
where

import CommandHelpers (addDirEntry, forgetDirEntryIfTracked, getDirEntry, makePathAbsolute,
                       rmDirEntry)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.State.Lazy
import qualified Data.ByteString.Char8 as BC
import Data.List (intercalate)
import FileSystem (Dir (..), File (..), buildFileWithContent, emptyDir, findDirentsBySubstring,
                   getChildCount, getFileMimeTypeByName, listDirEntries, showOptionalTime)
import PathUtils (lastSegment)
import RealIO (trackerSubdirName)
import ShellData (CommandException (..), ShellState (..))
import System.FilePath.Posix

lsCmd :: FilePath -> ExceptT CommandException (State ShellState) String
lsCmd path = do
  dirent <- getDirEntry path
  return $ case dirent of
    Left  _   -> takeFileName path
    Right dir -> intercalate "\n" (listDirEntries dir)

touchCmd :: FilePath -> ExceptT CommandException (State ShellState) String
touchCmd path = writeCmd path ""

mkdirCmd :: FilePath -> ExceptT CommandException (State ShellState) String
mkdirCmd path = do
  fullPath <- makePathAbsolute path
  when (lastSegment fullPath == trackerSubdirName)
       (throwError ReservedObjectName)
  let direntToWrite = Right emptyDir
  addDirEntry direntToWrite fullPath
  return ""

catCmd :: FilePath -> ExceptT CommandException (State ShellState) String
catCmd path = do
  dirent <- getDirEntry path
  case dirent of
    Right _    -> throwError IllegalObjectType
    Left  file -> return $ BC.unpack (fGetContent file)

rmCmd :: FilePath -> ExceptT CommandException (State ShellState) String
rmCmd path = do
  fullPath <- makePathAbsolute path
  rmDirEntry fullPath
  forgetDirEntryIfTracked fullPath
  return ""

writeCmd
  :: FilePath -> String -> ExceptT CommandException (State ShellState) String
writeCmd path text = do
  fullPath <- makePathAbsolute path
  when (lastSegment fullPath == trackerSubdirName)
       (throwError ReservedObjectName)
  let direntToWrite = Left $ buildFileWithContent text
  addDirEntry direntToWrite path
  return ""

findCmd :: String -> ExceptT CommandException (State ShellState) String
findCmd s = do
  dirent <- getDirEntry "."
  return $ intercalate "\n" (findDirentsBySubstring s dirent)

statCmd :: FilePath -> ExceptT CommandException (State ShellState) String
statCmd path = do
  fullPath <- makePathAbsolute path
  let pathLine = "Path: " ++ fullPath
  dirent <- getDirEntry path
  return $ intercalate "\n" $ case dirent of
    Left file -> [pathLine, permissionsLine, sizeLine, modLine, typeLine]     where
      permissionsLine = "Permissions: " ++ show (fGetPermissions file)
      sizeLine = "Size: " ++ show (fGetSize file)
      modLine = "Modified: " ++ showOptionalTime (fGetModificationTime file)
      typeLine = "Type: " ++ getFileMimeTypeByName (takeFileName fullPath)
    Right dir -> [pathLine, permissionsLine, sizeLine, fileCntLine]     where
      permissionsLine = "Permissions: " ++ show (dGetPermissions dir)
      sizeLine        = "Size: " ++ show (dGetSize dir)
      fileCntLine     = "Files: " ++ show (getChildCount dir)
