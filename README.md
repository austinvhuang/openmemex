# OpenMemex

*ATTENTION CONSERVATION NOTICE - this is an early stage project and is not recommended for use other than by contributing developers*

OpenMemex is an open source, local-first knowledge integration platform (aka "second brain" or "knowledge garden") optimized for automation (including caching and indexing of content) as well as enabling neural network machine learning integrations.

## What is OpenMemex for?

<img align="right" src="https://user-images.githubusercontent.com/20875313/136660771-3d32b50d-c4aa-46fb-9473-5d9c29f1b9b3.gif" alt="openmemex_demo" width="50%">

OpenMemex is designed to maximize a user's leverage as a brain cache/co-processor while minimizing curation friction. In contrast to productivity tools that require substantial user investment to organize and manage information:

- SQLite is the central data storage medium, rather than a collection of markdown documents.
- Instead of users manually curating a knowledge graph, data is organized automatically by timestamp. Topical/conceptual connections can be automatically linked by a combination of lightweight tagging and NLP models (WIP) rather than relying on the user to hand-curate relatedness.
- Although capturing markdown notes is in scope, the focus is on automated persistence, retrieval, and (future work) optimizing compression/consumption of information over UI-heavy notetaking tools.

Instead of focusing on developing an environment for a user to write, curate, and massage content, the goal is to enable a minimally-disruptive wiretap into one's (often-messy) stream of conscious intake and production of information, then automate organization/indexing for asynchronous future consumption.

## Contributing and Current Project State

The implementation is currently at functioning pre-alpha MVP maturity. As the note at the top says, it should primarily be used by contributors at this point.

There's lots of functionality to fill in and we're happy to have contributors join development, can DM [@austinvhuang on twitter](https://twitter.com/austinvhuang), message me on the Hasktorch slack server, or [join the OpenMemex discord](https://discord.gg/Afm4SVQn).

## Implementer Notes

The app is a self-hosted server (Haskell Servant) which hosts a web ui (Rust/Yew - wasm) and persists your data as a sqlite data store.

The central data structure is a time stamped event stream. Notes or links to external sites can be captured as events (eventually there may be other types of event data - audio/photos/etc). 

The general pattern of use is that the event stream is intended to persist both he intake and production of information. A link to a piece of information (e.g. a blog, a video, a paper, etc.) can be persisted as an event and it is automatically time stamped with the time of the capture. An idea or thought can also be captured as a note, with the note persisted as an event at the time it is written. Both links and notes can be annotated with relevant topic tags.

For links to external sites, a headless browser automatically caches the content of the link and stores it to create a local cached database of contents of all externally linked data. Besides the content cache and topic tags, each event has a few pieces (still-evolving) of additional metadata - a text note which can be edited to annotate the event, and a completion flag (intended to indicate that an event has bene "worked" by the user).

Events can be filtered and retrieved in three ways - search, time, and topics. Search functionality allows searching the stream (including locally cached content of links), time filters events by their time stamp, topic filters events by topic tags. 

# Project Organization

There are currently 3 main components.

- `frontend/` - this is the frontend user interface implementation. The frontend is implemented in rust + yew and compiles to wasm.
- `server/` - this is the backend implementation. It interacts with the sqlite database and in the future runs various automation tasks and (in the future) machine learning models.
- `shared/` - shared backend operations - more or less reused modules between `server` and the two supporting command line tools below (`cli/` and `crawler/`)

Additionally, there's two supporting command line tools (`omx` command line interface under `cli/` and `crawler` command line tool) which are mostly deprecated, except that the CLI is still needed on a first use to initialize the database table schemas.

- `cli/` - [[mostly deprecated except for initialization]] the command line tool. this is mostly no longer needed except to initialize the table schemas of the database (`omx --reset --note ""`), but can also be used to test adding notes at the command line eg `omx --note "this is a note" --tag "some_tag" --tag "another_tag`)
- `crawler/` - [[mostly deprecated]] for all notes consisting of urls, this crawls them, pulls html content into the database, but also takes screenshots, thumbnails, and runs ocr for a text representation of screenshots. This tool is also mostly deprecated in favor of running these operations synchronously upon adding a note instead of requiring users to run this process in batch on-demand.
- `electron/` - experimental Electron UI (not functioning yet).

There's also placeholder directories (consisting of a single `.gitkeep` file) where artifacts are intended to be stored:

- `screenshots/` - screenshots captured by the headless browser
- `thumbnails/` - scaled down version of screenshots
- `ocr/` - ocr output of screenshots

# Installation & Building

Warning - installation is still finicky at this stage. Ping @austinvhuang on twitter or on the hasktorch slack if you run into issues.

## System Dependencies

Make sure you have system dependencies:

- chromium (for headless browsing)
- imagemagick + libva-dev (for image processing - cropping, downsampling, etc. TODO - get rid of this dependency)
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
stack run omx --reset --note ""
```

This creates and initializes tables in the `openmemex.db` file which is the main backend data store.

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

You should be in the top level directory where openmemex.db is located when running the server.

To get to the main UI, point a browser to:

```
http://localhost:3000/frontend/index.html
```

# Things to do

See the [kanban board](https://github.com/austinvhuang/openmemex/projects/1)
