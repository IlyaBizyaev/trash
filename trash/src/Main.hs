-- SPDX-FileCopyrightText: 2020 Ilya Bizyaev <me@ilyabiz.com>
-- SPDX-License-Identifier: GPL-3.0+

module Main where

import           Options.Applicative            ( execParser )

import           Parsers                        ( CliOptions(..)
                                                , cliOptionsInfo
                                                )
import           Shell                          ( runREPL )
import           Gui                            ( runGUI )

-- TODO: reenable -Werror

main :: IO ()
main = do
  cliOptions <- execParser cliOptionsInfo

  case cliOptions of
    CliOptions True -> runGUI
    _               -> runREPL
