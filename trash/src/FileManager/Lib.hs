module FileManager.Lib
    ( createDir
    , removeDir
    -- , readFS
    -- , writeFS
    ) where

import System.Directory as SD
import FileSystem (Dir(..))

createDir :: Dir -> IO ()
createDir = SD.createDirectory . dGetPath

removeDir :: Dir -> IO ()
removeDir = SD.removeDirectoryRecursive . dGetPath

-- readFS :: FilePath -> FileSystem
-- readFS = undefined -- do

-- writeFS :: FileSystem -> IO ()
-- writeFS = undefined
