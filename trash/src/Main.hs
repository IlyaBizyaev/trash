-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module Main where

import           Options.Applicative           as OA
import           Data.Semigroup                 ( (<>) )
import           Data.Version                   ( showVersion )
import           Data.Char                      ( isSpace )
import           System.Directory              as SD
import           System.IO                      ( hFlush
                                                , stdout
                                                , hPutStrLn
                                                , stderr
                                                , hPrint
                                                )
import           FileSystem                     ( Dir(..)
                                                , readDirFromFilesystem
                                                , writeDirToFilesystem
                                                , isDirTracked
                                                )
import           Control.Monad.State.Lazy
import           Control.Monad.Except           ( ExceptT
                                                , runExceptT
                                                )
import System.FilePath.Posix
import           Paths_trash                    ( version )

data CliOptions = CliOptions
  { gui     :: Bool }

cliOptionsParser :: Parser CliOptions
cliOptionsParser =
  CliOptions <$> switch (long "gui" <> OA.help "Open in GUI mode")

cliOptionsInfo :: ParserInfo CliOptions
cliOptionsInfo = info
  (   helper
  <*> (infoOption (concat ["trash ", showVersion version])
                  (long "version" <> short 'v' <> help "Show version")
      )
  <*> cliOptionsParser
  )
  (  fullDesc
  <> progDesc "Start the shell in CLI or GUI mode, or print version and exit"
  <> header "trash - the tracker shell"
  )

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
                       | AddCommand FilePath
                       | LogCommand FilePath
                       | ForgetCommand FilePath
                       | CheckoutCommand FilePath Integer
                       | MergeCommand FilePath Integer Integer deriving (Eq, Show)
-- TODO: add strategy : String to MergeCommand
-- TODO: cvs-cat <file> "index" -- просмотр конкретной ревизии файла
-- TODO: cvs-delete-version <file> "index" -- удалить заданную версию файла из ревизий

newtype ParsedCommand = ParsedCommand { pGetCommand :: ShellCommand }

shellCommandParser :: Parser ShellCommand
shellCommandParser = hsubparser
  (  command "help" (info (pure HelpCommand) (progDesc "Display usage help"))
  <> command "exit" (info (pure ExitCommand) (progDesc "Exit the shell"))
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
         (progDesc "Create file with specified filename") -- TODO: behave like GNU touch?
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

shellCommandInfo :: ParserInfo ShellCommand
shellCommandInfo = info
  (helper <*> shellCommandParser)
  (fullDesc <> progDesc "Available shell commands" <> header
    "trash - the tracker shell"
  )

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
         (AddCommand <$> strArgument
           (metavar "FILE" <> help
             "Name of file or directory to add to check in to control"
           )
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
         (progDesc "Clear revision log of a file or a directory (recursively)"
         )
       )
  <> command
       "merge"
       (info
         (   MergeCommand
         <$> strArgument (metavar "FILE" <> help "Name of file to merge")
         <*> argument auto (metavar "REV" <> help "First revision number")
         <*> argument auto (metavar "REV" <> help "Second revision number")
         )
         (progDesc "Initiate merging of 2 file revisions")
       )
  )

parseResultAsEither :: ParserResult a -> Either String a
parseResultAsEither (Success a      ) = Right a
parseResultAsEither (Failure failure) = do
  let (msg, _) = renderFailure failure "tracker"
  Left msg
parseResultAsEither (CompletionInvoked _) = do
  Left "unreachable"

parseCommand :: String -> Either String ShellCommand
parseCommand s = case filter (not . isSpace) s of
  "" -> Right EmptyCommand
  _  -> parseResultAsEither result   where
    tokens = words s
    result = execParserPure defaultPrefs shellCommandInfo tokens


runGUI :: IO ()
runGUI = putStrLn "Unimplemented"

runREPL :: IO ()
runREPL = do
  initialPwd <- getCurrentDirectory
  printPrompt initialPwd
  initialDir <- readDirFromFilesystem initialPwd
  let initialTrackerDir = if isDirTracked initialDir then Just "." else Nothing
  stdinContents <- getContents
  let initialState = ShellState initialDir initialPwd initialTrackerDir
  printPrompt $ sGetPwd initialState
  let commands = lines stdinContents
  finalState <- execNextCommand commands initialState
  writeDirToFilesystem $ sGetRootDir finalState

execNextCommand :: [String] -> ShellState -> IO ShellState
execNextCommand []           st  = return st
execNextCommand (cmd : cmds) st = do
  case parseCommand cmd of
    Left msg -> do
      hPutStrLn stderr msg
      execNextCommand cmds st
    Right c -> case c of
      ExitCommand -> return st
      _           -> do
        let (res, newSt) = runState (runExceptT (execCommand c)) st
        case res of
          Left  e -> hPrint stderr e
          Right s -> putStrLn s
        execNextCommand cmds newSt
       where
        execCommand EmptyCommand             = undefined
        execCommand ExitCommand              = undefined
        execCommand HelpCommand              = helpCmd
        execCommand (LsCommand    path     ) = lsCmd path
        execCommand (CdCommand    path     ) = cdCmd path
        execCommand (TouchCommand path     ) = touchCmd path
        execCommand (MkdirCommand path     ) = mkdirCmd path
        execCommand (CatCommand   path     ) = catCmd path
        execCommand (RmCommand    path     ) = rmCmd path
        execCommand (WriteCommand path text) = writeCmd path text
        execCommand (FindCommand    s      ) = findCmd s
        execCommand (StatCommand    path   ) = statCmd path
        execCommand (TrackerCommand subc   ) = execTrackerSubcommand subc

        execTrackerSubcommand InitCommand                = initCmd
        execTrackerSubcommand (AddCommand    path      ) = addCmd path
        execTrackerSubcommand (LogCommand    path      ) = logCmd path
        execTrackerSubcommand (ForgetCommand path      ) = forgetCmd path
        execTrackerSubcommand (CheckoutCommand path rev) = checkoutCmd path rev
        execTrackerSubcommand (MergeCommand path rev1 rev2) =
          mergeCmd path rev1 rev2

printPrompt :: FilePath -> IO ()
printPrompt pwd = do
  putStr $ pwd ++ "> "
  hFlush stdout

data ShellState = ShellState {
  sGetRootDir :: Dir,
  sGetPwd :: FilePath,
  sGetTrackerDir :: Maybe FilePath
}

data CommandException = UnknownException deriving (Eq, Show)

-- emptyCmd :: ExceptT CommandException (State ShellState) String
-- emptyCmd = return ""

helpCmd :: ExceptT CommandException (State ShellState) String
helpCmd = return "TODO: help text"

lsCmd :: FilePath -> ExceptT CommandException (State ShellState) String
lsCmd path = undefined -- use getDirentryByPath, then process error message (wrap) or check type or list map keys
-- do not forget to combine given path with existing relative path

cdCmd :: FilePath -> ExceptT CommandException (State ShellState) String
cdCmd path = undefined -- check subdir exists (use getDirentryByPath), modify relative path

touchCmd :: FilePath -> ExceptT CommandException (State ShellState) String
touchCmd path = writeCmd path ""

mkdirCmd :: FilePath -> ExceptT CommandException (State ShellState) String
mkdirCmd path = undefined -- same as "touch", just different direntry type

catCmd :: FilePath -> ExceptT CommandException (State ShellState) String
catCmd path = undefined -- get if exists, dump contents to res string

rmCmd :: FilePath -> ExceptT CommandException (State ShellState) String
rmCmd path = undefined -- same as touch/mkdir, but with reversed check
-- TODO: how to handle removal of tracked files? Forget? Or think of a way to handle as a revision?
-- Special mark?

-- if dirent does not exist AND we know it's a valid filename (how?), create the file
writeCmd
  :: FilePath -> String -> ExceptT CommandException (State ShellState) String
writeCmd path text = undefined

findCmd :: String -> ExceptT CommandException (State ShellState) String
findCmd s = undefined -- get location referenced by current rel path, then recursively check filenames (helper?)

statCmd :: FilePath -> ExceptT CommandException (State ShellState) String
statCmd path = undefined -- get location, then print fields according to DirEntry type; consider Maybes

initCmd :: ExceptT CommandException (State ShellState) String
initCmd = undefined -- verify TrackerData is None; create it with 0 and Map.empty

addCmd :: FilePath -> ExceptT CommandException (State ShellState) String
addCmd path = undefined -- verify TrackerData is Just; verify dirent exists; be recursive with dirs; copy file
-- contents to revisions, incrementing rev counter

logCmd :: FilePath -> ExceptT CommandException (State ShellState) String
logCmd path = undefined -- get all revisions of file / recurively in existing dir and vcs
-- sort them, print them

forgetCmd :: FilePath -> ExceptT CommandException (State ShellState) String
forgetCmd path = undefined -- check that the path is checked in, in which case delete map key

checkoutCmd
  :: FilePath -> Integer -> ExceptT CommandException (State ShellState) String
checkoutCmd path rev = undefined -- oof, this one is hard... check that the path is tracked
-- if we choose to forget files we delete, then existing revision means existing file/dir; replace contents
-- [recursively]
-- otherwise, need to code creating of missing dirents

mergeCmd
  :: FilePath
  -> Integer
  -> Integer
  -> ExceptT CommandException (State ShellState) String
mergeCmd path rev1 rev2 = undefined -- in fact, is only different from checkoutCmd due to existing comparison
-- maybe checkoutCmd can be expressed through mergeCmd with a special case for equal revision nums
-- sounds good to me!

main :: IO ()
main = do
  cliOptions <- execParser cliOptionsInfo

  case cliOptions of
    CliOptions True -> runGUI
    _               -> runREPL
