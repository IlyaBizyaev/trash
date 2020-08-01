# Trash — The Tracker Shell
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://github.com/IlyaBizyaev/trash/blob/master/LICENSE)
![Haskell Stack CI](https://github.com/IlyaBizyaev/trash/workflows/Haskell%20Stack%20CI/badge.svg)

This repository contains a small shell implementation that has some basic file management commands and a tiny VCS called Tracker.
Inspired by crosh.

## Usage
### Running
```bash
$ stack run
```

### Commands
**File management:** use `-h` in REPL for a list of supported FM commands.

**Version control:** use `tracker -h` in REPL for a list of supported VCS commands.

## Development
### Building
```bash
$ stack build
```

### Testing
Sadly, I couldn't manage to implement a test suite in time.

## Copyright
2020 Ilya Bizyaev <me@ilyabiz.com>, GPL 3+
