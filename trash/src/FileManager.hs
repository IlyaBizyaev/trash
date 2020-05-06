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

import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                )
import           Control.Monad.State.Lazy
import qualified Data.ByteString.Char8         as BC
import           FileSystem                     ( Dir(..)
                                                , File(..)
                                                , emptyDir
                                                , findDirentsBySubstring
                                                , showOptionalTime
                                                , getFileMimeTypeByName
                                                , buildFileWithContent
                                                , listDirEntries
                                                , getChildCount
                                                )
import           ShellData                      ( ShellState(..)
                                                , CommandException(..)
                                                )
import           System.FilePath.Posix
import           Data.List                      ( intercalate )
import           CommandHelpers                 ( getDirentryByPath
                                                , isPathAbsent
                                                , addDirEntry
                                                , rmDirEntry
                                                , isFileTracked
                                                , forgetDirEntry
                                                )


lsCmd :: FilePath -> ExceptT CommandException (State ShellState) String
lsCmd path = do
  dirent <- getDirentryByPath path
  return $ case dirent of
    Left  _   -> takeFileName path
    Right dir -> intercalate "\n" (listDirEntries dir)

touchCmd :: FilePath -> ExceptT CommandException (State ShellState) String
touchCmd path = writeCmd path ""

mkdirCmd :: FilePath -> ExceptT CommandException (State ShellState) String
mkdirCmd path = do
  let normalizedPath = normalise path -- Not enough, need to solve .. etc.
  -- ^ TODO: attempt to reduce duplication
  unless (isValid normalizedPath) (throwError UnknownException)
  isAbsent <- isPathAbsent normalizedPath
  unless isAbsent (throwError UnknownException)
  let direntToWrite = Right emptyDir
  addDirEntry direntToWrite path
  return ""

catCmd :: FilePath -> ExceptT CommandException (State ShellState) String
catCmd path = do
  dirent <- getDirentryByPath path
  case dirent of
    Right _    -> throwError UnknownException
    Left  file -> return $ BC.unpack (fGetContent file)

rmCmd :: FilePath -> ExceptT CommandException (State ShellState) String
rmCmd path = do
  let normalizedPath = normalise path -- Not enough, need to solve .. etc.
  unless (isValid normalizedPath) (throwError UnknownException)
  isAbsent <- isPathAbsent normalizedPath
  when isAbsent (throwError UnknownException)
  rmDirEntry path
  fileTracked <- isFileTracked path
  when fileTracked (forgetDirEntry path)
  return ""

writeCmd
  :: FilePath -> String -> ExceptT CommandException (State ShellState) String
writeCmd path text = do
  let normalizedPath = normalise path -- Not enough, need to solve .. etc.
  unless (isValid normalizedPath) (throwError UnknownException)
  isAbsent <- isPathAbsent normalizedPath
  unless isAbsent (throwError UnknownException)
  let direntToWrite = Left $ buildFileWithContent text
  addDirEntry direntToWrite path
  return ""

findCmd :: String -> ExceptT CommandException (State ShellState) String
findCmd s = do
  st <- get
  let pwd = sGetPwd st
  dirent <- getDirentryByPath pwd
  return $ intercalate "\n" (findDirentsBySubstring s dirent)

statCmd :: FilePath -> ExceptT CommandException (State ShellState) String
statCmd path = do
  let normalizedPath = normalise path -- Not enough, need to solve .. etc.
  let fullPath       = normalizedPath -- TODO: need to use </>, but what if passed path is already absolute?
  -- need to fix this everywhere
  let pathLine       = "Path: " ++ fullPath
  dirent <- getDirentryByPath normalizedPath
  return $ intercalate "\n" $ case dirent of
    Left file ->
      [pathLine, permissionsLine, sizeLine, modLine, creationLine, typeLine]     where
      permissionsLine = "Permissions: " ++ show (fGetPermissions file)
      sizeLine = "Size: " ++ show (fGetSize file)
      modLine = "Modified: " ++ showOptionalTime (fGetModificationTime file)
      creationLine = "Created: " ++ showOptionalTime (fGetCreationTime file)
      typeLine = "Type: " ++ getFileMimeTypeByName (takeFileName fullPath)
    Right dir -> [pathLine, permissionsLine, sizeLine, fileCntLine]     where
      permissionsLine = "Permissions: " ++ show (dGetPermissions dir)
      sizeLine        = "Size: " ++ show (dGetSize dir)
      fileCntLine     = "Files: " ++ show (getChildCount dir)
