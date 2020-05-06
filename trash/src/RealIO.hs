-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module RealIO (readDirFromFilesystem, writeDirToFilesystem) where

import FileSystem (Dir(..))

-- .tracker subdir

-- use calculateSize
readDirFromFilesystem :: FilePath -> IO Dir
readDirFromFilesystem path = undefined

writeDirToFilesystem :: Dir -> IO ()
writeDirToFilesystem dir = undefined
