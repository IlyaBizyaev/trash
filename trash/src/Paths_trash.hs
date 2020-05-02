-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module Paths_trash
  ( version
  )
where
import           Data.Version                   ( Version
                                                , makeVersion
                                                )

version :: Version
version = makeVersion [0]
