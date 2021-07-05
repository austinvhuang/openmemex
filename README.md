# OpenMemex

WARNING - not recommended for use other than development.

# Project Organization

There are currently 3 main components.

- `frontend-rs/` - this is the frontend user interface implementation, uses rust + yew compiles to wasm.
- `server/` - this is the backend server, interacts with the database and in the future runs various automation tasks and (in the future) models.
- `shared/` - shared backend operations - more or less reused modules between `server` and the two supporting command line tools below (`cli/` and `crawler/`)

Additionally, there's two supporting command line tools (`n2s` command line interface under `cli/` and `crawler` command line tool) which are mostly deprecated, except that the CLI is still needed on a first use to initialize the database table schemas.

- `cli/` - [[mostly deprecated except for initialization]] the command line tool. this is mostly no longer needed except to initialize the table schemas of the database (`n2s --reset --note ""`), but can also be used to test adding notes at the command line eg `n2s --note "blah blah" --tag "atag" --tag "anothertag`)
- `crawler/` - [[mostly deprecated]] for all notes consisting of urls, this crawls them, pulls html content into the database, but also takes screenshots, thumbnails, and runs ocr for a text representation of screenshots. This tool is also mostly deprecated in favor of running these operations synchronously upon adding a note instead of requiring users to run this process in batch on-demand (which leaves part of the database un-crawled).
- `electron/` - experimental Electron UI (not functioning yet)

There's also 2 directories where some artifacts are stored:

- `screenshots/` - screenshots captured by the headless browser
- `thumbnails/` - scaled down version of screenshots
- `ocr/` - tesseract ocr output of screenshots (in the future this might be derived from a higher-resolution image of the full site)

# Installation & Building

Warning - installation is still a bit finicky at this point. Ping me on the hasktorch slack if you run into issues.

## System Dependencies

Make sure you have system dependencies:

- chromium (for headless browsing)
- imagemagick + libva-dev (for image processing - cropping, downsampling, etc.)
- tesseract + libtesseract-dev (ocr for screenshots)

On linux, run:

`make install-dependencies`

This runs `apt-get` and `snap` to install the above packages.

## Haskell Tools

Install ormolu and ghcid:

`make install-haskell-tools`

This runs `stack install ormolu ghcid`

## Shared libraries - libtorch & tokenizers

Libtorch is a shared runtime library dependency. 

For now the cpu build is probably sufficient since most operations are model inference.

`make libtorch/lib/libtorch_cpu.so`

Currently tokenizers has to be copied manually or symbolically linked locally.

Under `deps/tokenizers/`, copy or link `libtokenizers_haskell.so` which is built from the [hasktorch fork of the huggingface tokenizers library](https://github.com/hasktorch/tokenizers)

## Environment variables

Set shared library paths:

`source setenv`

## Initializing the database

TODO: Web API version of this functionality

For now, the way to initialize the database is with the command line tool:

```
stack build cli
stack run n2s --reset --note ""
```

This creates and initializes tables in the `note2self.db` file which is the main backend data store.

## Web frontend and API server

First build the wasm artifact:

```
cd frontend-rs
make
```

This creates `wasm_bg.wasm` in the `frontend-rs/static` directory. The `frontend-rs/static` is hosted by the servant server.

Setup environment variables with `source setenv` if you haven't already and start the server:

```
stack run server
```

# Things to do

See the [kanban board](https://github.com/austinvhuang/note2self/projects/1)
