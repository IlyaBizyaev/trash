-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module Parsers
  ( CliOptions(..)
  , cliOptionsInfo
  , parseCommand
  )
where

import Data.Char (isSpace)
import Data.Semigroup ((<>))
import Options.Applicative
import ShellData (ShellCommand (..), TrackerSubcommand (..), shellVersionAndCodename)

-- | trash's command line arguments.
data CliOptions = CliOptions
  { gui     :: Bool }

-- | trash's CLI arg parser.
cliOptionsParser :: Parser CliOptions
cliOptionsParser =
  CliOptions <$> switch (long "gui" <> help "Open in GUI mode")

-- | trash CLI help description.
cliOptionsInfo :: ParserInfo CliOptions
cliOptionsInfo = info
  (   helper
  <*> (infoOption shellVersionAndCodename
                  (long "version" <> short 'v' <> help "Show version")
      )
  <*> cliOptionsParser
  )
  (  fullDesc
  <> progDesc "Start the shell in CLI or GUI mode, or print version and exit"
  <> header "trash - the tracker shell"
  )

-- | trash's REPL command parser.
shellCommandParser :: Parser ShellCommand
shellCommandParser = hsubparser
  (  command "exit" (info (pure ExitCommand) (progDesc "Exit the shell"))
  <> command
       "debug"
       (info (pure DebugCommand)
             (progDesc "Display internal filesystem state")
       )
  <> command
       "cd"
       (info
         (   CdCommand
         <$> strArgument (metavar "DIR" <> help "Directory to go to")
         )
         (progDesc "Change present working directory")
       )
  <> command
       "ls"
       (info
         (LsCommand <$> strArgument
           (metavar "DIR" <> help "Directory to list contents of")
         )
         (progDesc "List contents of specified directory")
       )
  <> command
       "touch"
       (info
         (TouchCommand <$> strArgument
           (metavar "FILE" <> help "Filename of file to create")
         )
         (progDesc "Create file with specified name")
       )
  <> command
       "mkdir"
       (info
         (MkdirCommand <$> strArgument
           (metavar "DIR" <> help "Filename of directory to create")
         )
         (progDesc "Create directory with specified filename")
       )
  <> command
       "cat"
       (info
         (CatCommand <$> strArgument
           (metavar "FILE" <> help "Display contents of the specified file")
         )
         (progDesc "Print file with specified filename")
       )
  <> command
       "rm"
       (info
         (   RmCommand
         <$> strArgument
               (  metavar "FILE"
               <> help "Filename of file or directory to remove"
               )
         )
         (progDesc "Remove file or directory with specified filename")
       )
  <> command
       "write"
       (info
         (   WriteCommand
         <$> strArgument
               (metavar "FILE" <> help "Filename of file to write to")
         <*> strArgument (metavar "TEXT" <> help "Text to write")
         )
         (progDesc "Create file with specified filename and contents")
       )
  <> command
       "find"
       (info
         (FindCommand <$> strArgument
           (metavar "TEXT" <> help "Filename substring to search for")
         )
         (progDesc "Find files with the specified substring in name")
       )
  <> command
       "stat"
       (info
         (StatCommand <$> strArgument
           (metavar "FILE" <> help
             "Filename of file or directory to get information about"
           )
         )
         (progDesc "Display information about a file or directory")
       )
  <> command
       "tracker"
       (info (TrackerCommand <$> (helper <*> trackerSubcommandParser))
             (progDesc "Invokes tracker, the version control system")
       )
  )

-- | trash's REPL parser help info.
shellCommandInfo :: ParserInfo ShellCommand
shellCommandInfo = info
  (helper <*> shellCommandParser)
  (fullDesc <> progDesc "Available shell commands" <> header
    "trash - the tracker shell"
  )

-- | tracker command parser.
trackerSubcommandParser :: Parser TrackerSubcommand
trackerSubcommandParser = hsubparser
  (  command
      "init"
      (info (pure InitCommand)
            (progDesc "Init Tracker VCS in current directory")
      )
  <> command
       "add"
       (info
         (   AddCommand
         <$> strArgument
               (metavar "FILE" <> help
                 "Name of file or directory to add to check in to control"
               )
         -- TODO: Allow input with spaces (implement quoted text parser)
         <*> strArgument (metavar "SUMMARY" <> help "Summary of the change")
         )
         (progDesc "Add file or directory as revision to Tracker")
       )
  <> command
       "log"
       (info
         (LogCommand <$> strArgument
           (  metavar "FILE"
           <> help "Name of file or directory to display log of"
           )
         )
         (progDesc
           "Display revision log of a file or a directory (recursively)"
         )
       )
  <> command
       "forget"
       (info
         (ForgetCommand <$> strArgument
           (  metavar "FILE"
           <> help "Name of file or directory to clear revision log of"
           )
         )
         (progDesc "Clear revision log of a file or a directory (recursively)"
         )
       )
  <> command
       "forget-rev"
       (info
         (   ForgetRevCommand
         <$> strArgument
               (metavar "FILE" <> help
                 "Name of file or directory to delete a revision of"
               )
         <*> argument auto
                      (metavar "REV" <> help "Revision number to delete")
         )
         (progDesc "Delete specific revision of a tracked file")
       )
  <> command
       "checkout"
       (info
         (   CheckoutCommand
         <$> strArgument
               (metavar "FILE" <> help
                 "Name of file or directory to clear revision log of"
               )
         <*> argument
               auto
               (metavar "REV" <> help "Revision number to check out")
         )
         (progDesc "Pring content of file at specific revision")
       )
  <> command
       "merge"
       (info
         (   MergeCommand
         <$> strArgument (metavar "FILE" <> help "Name of file to merge")
         <*> argument auto (metavar "REV" <> help "First revision number")
         <*> argument auto (metavar "REV" <> help "Second revision number")
         <*> strArgument
               (  metavar "STRATEGY"
               <> help "Merge strategy: left, right or both"
               )
         )
         (progDesc "Initiate merging of 2 file revisions")
       )
  )

-- | A pure parse result handler (stub).
parseResultAsEither :: ParserResult a -> Either String a
parseResultAsEither (Success a      ) = Right a
parseResultAsEither (Failure failure) = do
  let (msg, _) = renderFailure failure "tracker"
  Left msg
parseResultAsEither (CompletionInvoked _) = do
  Left "unreachable"

-- | trash REPL parser wrapper.
parseCommand :: String -> Either String ShellCommand
parseCommand s = case filter (not . isSpace) s of
  "" -> Right EmptyCommand
  _  -> parseResultAsEither result   where
    tokens = words s
    result = execParserPure defaultPrefs shellCommandInfo tokens
