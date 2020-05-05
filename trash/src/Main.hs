-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module Main where

import           Options.Applicative           as OA
import           Data.Semigroup                 ( (<>) )
import           Data.Version                   ( showVersion )
import           Data.Char                      ( isSpace )
import           Data.List                      ( intercalate )
import           System.Directory              as SD
import           System.IO                      ( hFlush
                                                , stdout
                                                , hPutStrLn
                                                , stderr
                                                , hPrint
                                                )
import           FileSystem                     ( Dir(..)
                                                , File(..)
                                                , DirEntry
                                                , readDirFromFilesystem
                                                , writeDirToFilesystem
                                                , isDirTracked
                                                , listDirEntries
                                                , getDirentryByFullPath
                                                , buildFileWithContent
                                                , addDirEntry
                                                , rmDirEntry
                                                , emptyDir
                                                , isFileTrackedInDir
                                                , findDirentsBySubstring
                                                , getChildCount
                                                , getFileMimeTypeByName
                                                , showOptionalTime
                                                , emptyTrackerData
                                                )
import           Control.Monad.State.Lazy
import           Control.Monad.Except           ( ExceptT
                                                , runExceptT
                                                , throwError
                                                )
import           System.FilePath.Posix
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
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
                       | ForgetRevCommand FilePath Integer
                       | CheckoutCommand FilePath Integer
                       | MergeCommand FilePath Integer Integer String deriving (Eq, Show)

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
       "forget-rev"
       (info
         (ForgetRevCommand <$> strArgument
           (  metavar "FILE"
           <> help "Name of file or directory to delete a revision of"
           )
           <*> argument
               auto
               (metavar "REV" <> help "Revision number to delete")
         )
         (progDesc "Delete specific revision of a tracked file"
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
         <*> strArgument (metavar "STRATEGY" <> help "Merge strategy: left, right or both")
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
  let initialTrackerDir = if isDirTracked initialDir then Just "/" else Nothing
  stdinContents <- getContents
  let initialState = ShellState initialDir initialPwd initialTrackerDir
  printPrompt initialPwd
  let commands = lines stdinContents
  finalState <- execNextCommand commands initialState
  writeDirToFilesystem $ sGetRootDir finalState

execNextCommand :: [String] -> ShellState -> IO ShellState
execNextCommand []           st = return st
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
        execTrackerSubcommand (ForgetRevCommand path rev) = forgetRevCmd path rev
        execTrackerSubcommand (CheckoutCommand path rev) = checkoutCmd path rev
        execTrackerSubcommand (MergeCommand path rev1 rev2 strategy) =
          mergeCmd path rev1 rev2 strategy

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

rmDirEntryFromState :: FilePath -> ExceptT CommandException (State ShellState) ()
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
    Nothing -> throwError UnknownException
    Just tDir -> do
      trackerDirent <- getDirentryByPath tDir
      case trackerDirent of
        Left _ -> throwError UnknownException
        Right dir -> return $ isFileTrackedInDir dir path

updatePwd :: FilePath -> ShellState -> ShellState
updatePwd path st = st { sGetPwd = newPwd, sGetTrackerDir = newTrackerDir } where
  rootDir          = sGetRootDir st
  oldPwd           = sGetPwd st
  newPwd           = oldPwd </> path -- TODO: somehow normalize here, or the result can make all kinds of weird things
  newPwdComponents = (reverse . splitDirectories) newPwd
  newTrackerDir    = findClosestTrackedAncestor newPwdComponents
  findClosestTrackedAncestor []         = Nothing
  findClosestTrackedAncestor x@(_ : xs) = if curPathTracked
    then Just curPath
    else ancestorCheck   where
    curPath        = (joinPath . reverse) x
    curPathTracked = isSubdirAtPathTracked curPath
    ancestorCheck  = findClosestTrackedAncestor xs
  isSubdirAtPathTracked p = case getDirentryByFullPath rootDir p of
    Nothing          -> False
    Just (Left  _  ) -> False
    Just (Right dir) -> isDirTracked dir

helpCmd :: ExceptT CommandException (State ShellState) String
helpCmd = return
  "TODO: help text, or better learn to make optparse-applicative display this"

lsCmd :: FilePath -> ExceptT CommandException (State ShellState) String
lsCmd path = do
  dirent <- getDirentryByPath path
  return $ case dirent of
    Left  _   -> takeFileName path
    Right dir -> intercalate "\n" (listDirEntries dir)

cdCmd :: FilePath -> ExceptT CommandException (State ShellState) String
cdCmd path = do
  dirent <- getDirentryByPath path
  case dirent of
    Left  _ -> throwError UnknownException
    Right _ -> do
      modify (updatePwd path)
      return ""

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
  addDirEntryToState direntToWrite path
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
  rmDirEntryFromState path
  fileTracked <- isFileTracked path
  if fileTracked then forgetCmd path else return ""

writeCmd
  :: FilePath -> String -> ExceptT CommandException (State ShellState) String
writeCmd path text = do
  let normalizedPath = normalise path -- Not enough, need to solve .. etc.
  unless (isValid normalizedPath) (throwError UnknownException)
  isAbsent <- isPathAbsent normalizedPath
  unless isAbsent (throwError UnknownException)
  let direntToWrite = Left $ buildFileWithContent text
  addDirEntryToState direntToWrite path
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
  let fullPath = normalizedPath -- TODO: need to use </>, but what if the passed path is already absolute?
  -- need to fix this everywhere
  let pathLine = "Path: " ++ fullPath
  dirent <- getDirentryByPath normalizedPath
  return $ intercalate "\n" $ case dirent of
    Left file -> [pathLine, permissionsLine, sizeLine, modLine, creationLine, typeLine] where
      permissionsLine = "Permissions: " ++ show (fGetPermissions file)
      sizeLine = "Size: " ++ show (fGetSize file)
      modLine = "Modified: " ++ showOptionalTime (fGetModificationTime file)
      creationLine = "Created: " ++ showOptionalTime (fGetCreationTime file)
      typeLine = "Type: " ++ getFileMimeTypeByName (takeFileName fullPath)
    Right dir -> [pathLine, permissionsLine, sizeLine, fileCntLine] where
      permissionsLine = "Permissions: " ++ show (dGetPermissions dir)
      sizeLine = "Size: " ++ show (dGetSize dir)
      fileCntLine = "Files: " ++ show (getChildCount dir)

initCmd :: ExceptT CommandException (State ShellState) String
initCmd = do
  dirent <- getDirentryByPath "."
  case dirent of
    Left _ -> throwError UnknownException
    Right dir -> if isDirTracked dir then throwError UnknownException else do
      return ""
-- learn to put emptyTrackerData deep into the structure, and how to generalize it for other funcs...
-- at least the action of VCS replacement

addCmd :: FilePath -> ExceptT CommandException (State ShellState) String
addCmd path = undefined
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
forgetCmd path = undefined
-- function on state's tracker dir data:
-- * if no tracker dir, fail with error
-- * if have tracker dir but not file's revision log in it, fail with error
-- * otherwise delete file's key from tracker dir data
-- along the way, need file path relative to tracker dir (child/eq, or fail)

forgetRevCmd :: FilePath -> Integer -> ExceptT CommandException (State ShellState) String
forgetRevCmd path rev = undefined
-- function on state's tracker dir data:
-- * if no tracker dir, fail with error
-- * if have tracker dir but not file's revision log in it, fail with error
-- * if have file revision log but not this rev in it, fail with error
-- * otherwise delete file's key from tracker dir data
-- along the way, need file path relative to tracker dir (child/eq, or fail)

checkoutCmd
  :: FilePath -> Integer -> ExceptT CommandException (State ShellState) String
checkoutCmd path rev = undefined
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
-- ensure vcs dir is not Nothing
-- get full path of the target path
-- verify full path is a child/eq of tracker path
-- get that path relative to tracker path
-- get dirent for path (normalized relative? or works with absolute?)
-- get vcs dir
-- get tracker data
-- for file dirent: query specified revs, fail if one is not present, choose one based on strategy, write to file
-- for dir dirent: display error message

main :: IO ()
main = do
  cliOptions <- execParser cliOptionsInfo

  case cliOptions of
    CliOptions True -> runGUI
    _               -> runREPL
