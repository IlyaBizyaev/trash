-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module ShellData
  ( ShellCommand(..)
  , TrackerSubcommand(..)
  , ShellState(..)
  , CommandException(..)
  , shellVersionAndCodename
  )
where

import Data.List (intercalate)

import FileSystem (Dir (..))

shellVersionAndCodename :: String
shellVersionAndCodename = "v0.1.0 tragic speshka"

-- | Enumeration of available trash commands.
data ShellCommand = EmptyCommand
                  | ExitCommand
                  | DebugCommand
                  | CdCommand FilePath
                  | LsCommand FilePath
                  | TouchCommand FilePath
                  | MkdirCommand FilePath
                  | CatCommand FilePath
                  | RmCommand FilePath
                  | WriteCommand FilePath String
                  | FindCommand String
                  | StatCommand FilePath
                  | TrackerCommand TrackerSubcommand deriving (Eq, Show)

-- | Enumeration of available tracker commands.
data TrackerSubcommand = InitCommand
                       | AddCommand FilePath String
                       | LogCommand FilePath
                       | ForgetCommand FilePath
                       | ForgetRevCommand FilePath Integer
                       | CheckoutCommand FilePath Integer
                       | MergeCommand FilePath Integer Integer String deriving (Eq, Show)

-- | Data type for the shell cross-command execution state.
data ShellState = ShellState {
  sGetRootDir    :: Dir,
  sGetPwd        :: FilePath,
  sGetTrackerDir :: Maybe FilePath
}

instance Show ShellState where
  show (ShellState dir pwd trackerPath) = intercalate
    "\n"
    ["PWD: " ++ pwd, "Tracker: " ++ show trackerPath, "FS: " ++ show dir]

-- | Enumeration of exceptions thrown by shell commands.
data CommandException = ReservedObjectName
                      | IllegalObjectType
                      | LocationNotTracked
                      | ObjectNotFound
                      | ObjectIsNotAChild
                      | RevisionIsNotInTrackerData
                      | FileIsNotInTrackerData
                      | ShellInternalError
                      | ObjectAlreadyExists
                      | DirectoryAlreadyTracked
                      | UnknownMergeStrategy deriving (Eq, Show)
-- TODO: implement Show for CommandException manually, extend exceptions with context.
