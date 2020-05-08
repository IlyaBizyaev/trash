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

import FileSystem (Dir (..))

shellVersionAndCodename :: String
shellVersionAndCodename = "v0.1.0 tragic speshka"

data ShellCommand = EmptyCommand
                  | HelpCommand
                  | ExitCommand
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

data TrackerSubcommand = InitCommand
                       | AddCommand FilePath String
                       | LogCommand FilePath
                       | ForgetCommand FilePath
                       | ForgetRevCommand FilePath Integer
                       | CheckoutCommand FilePath Integer
                       | MergeCommand FilePath Integer Integer String deriving (Eq, Show)

data ShellState = ShellState {
  sGetRootDir    :: Dir,
  sGetPwd        :: FilePath,
  sGetTrackerDir :: Maybe FilePath
}

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
