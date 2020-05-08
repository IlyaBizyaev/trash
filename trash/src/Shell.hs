-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module Shell
  ( ShellCommand(..)
  , TrackerSubcommand(..)
  , runREPL
  )
where
import CommandHelpers (getDirEntry, makePathAbsolute, updateTrackerPath)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State.Lazy
import FileManager (catCmd, findCmd, lsCmd, mkdirCmd, rmCmd, statCmd, touchCmd, writeCmd)
import FileSystem (isDirTracked)
import Parsers (parseCommand)
import RealIO (readDirEntryFromFilesystem, writeDirEntryToFilesystem)
import ShellData (CommandException (..), ShellCommand (..), ShellState (..), TrackerSubcommand (..))
import qualified System.Directory as SD
import System.IO (hFlush, hPrint, hPutStrLn, stderr, stdout)
import Tracker (addCmd, checkoutCmd, forgetCmd, forgetRevCmd, initCmd, logCmd, mergeCmd)

runREPL :: IO ()
runREPL = do
  initialPwd <- SD.getCurrentDirectory
  printPrompt initialPwd
  initialDir <- readDirEntryFromFilesystem initialPwd
  case initialDir of
    Left  _     -> hPutStrLn stderr "unreachable"
    Right initD -> do
      let initialTrackerDir = if isDirTracked initD then Just "/" else Nothing
      stdinContents <- getContents
      let initialState = ShellState initD initialPwd initialTrackerDir
      printPrompt initialPwd
      let commands = lines stdinContents
      finalState <- execNextCommand commands initialState
      writeDirEntryToFilesystem initialPwd $ Right $ sGetRootDir finalState

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
          Left e -> do
            hPrint stderr e
            hFlush stderr
          Right s -> do
            putStrLn s
            printPrompt (sGetPwd newSt)
        execNextCommand cmds newSt
       where
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
        execCommand _                        = undefined

        execTrackerSubcommand InitCommand               = initCmd
        execTrackerSubcommand (AddCommand path summary) = addCmd path summary
        execTrackerSubcommand (LogCommand    path     ) = logCmd path
        execTrackerSubcommand (ForgetCommand path     ) = forgetCmd path
        execTrackerSubcommand (ForgetRevCommand path rev) =
          forgetRevCmd path rev
        execTrackerSubcommand (CheckoutCommand path rev) = checkoutCmd path rev
        execTrackerSubcommand (MergeCommand path rev1 rev2 strategy) =
          mergeCmd path rev1 rev2 strategy

printPrompt :: FilePath -> IO ()
printPrompt pwd = do
  putStr $ pwd ++ " > "
  hFlush stdout

updatePwd :: FilePath -> ShellState -> ShellState
updatePwd newPwd st = updateTrackerPath st { sGetPwd = newPwd }

helpCmd :: ExceptT CommandException (State ShellState) String
helpCmd = return
  "TODO: help text, or better learn to make optparse-applicative display this"

cdCmd :: FilePath -> ExceptT CommandException (State ShellState) String
cdCmd path = do
  fullPath <- makePathAbsolute path
  dirent   <- getDirEntry path
  case dirent of
    Left  _ -> throwError IllegalObjectType
    Right _ -> do
      modify (updatePwd fullPath)
      return ""
