# OpenMemex

*ATTENTION CONSERVATION NOTICE - this is an early stage project and is not recommended for use other than by contributing developers*

OpenMemex is an open source, local-first knowledge automation app.

# What is OpenMemex for?

There's a lot of "knowledge garden" tools that don't work for me for the same reason that real gardens don't work for me. Both *should* make your life more pleasant but in reality they just create more work because you need to maintain them.

A core value of OpenMemex is that a memex should extend your brain like a co-processor + cache,  *OpenMemex is designed to maximize a user's leverage*.

- Instead of markdown documents being the central data store, we use sqlite.
- Instead of the user organizing the structure of ideas by curating a knowledge graph, data is organized automatically by timestamp. Topical/conceptual connections will be automated by a combination of lightweight tagging and machine learning capabilities, rather than relying on the user to hand-curate cross references.
- While authoring notes is part of the functionality, the focus is on effective automated persistence, retrieval, and (future work) optimizing compression/consumption of information.

Instead of focusing on developing an environment for you to write, curate, and massage content for extended periods of time, the goal is to wiretap into one's (often-messy) stream of conscious intake and production of information, then automate the organization of it and surface it for asynchronous consumption/retrieval when needed at a future point in time.

# Implementation Notes

The app is a self-hosted server (haskell) which hosts a web ui (rust/yew/wasm) and persists your data as a sqlite data store.

The central data structure is a time stamped event stream. Notes or links to external sites can be captured as events (eventually there may be other types of event data - audio/photos/etc). 

The general pattern of use is that the event stream is intended to persist both he intake and production of information. A link to a piece of information (e.g. a blog, a video, a paper, etc.) can be persisted as an event and it is automatically time stamped with the time of the capture. An idea or thought can also be captured as a note, with the note persisted as an event at the time it is written. Both links and notes can be annotated with relevant topic tags.

For links to external sites, a headless browser automatically caches the content of the link and stores it to create a local cached database of contents of all externally linked data. Besides the content cache and topic tags, each event has a few pieces (still-evolving) of additional metadata - a text note which can be edited to annotate the event, and a completion flag (intended to indicate that an event has bene "worked" by the user).

Events can be filtered and retrieved in three ways - search, time, and topics. Search approximates a google-like search over the stream, time filters events by their time stamp, topic filters events by topic tags. 

# Project Organization

There are currently 3 main components.

- `frontend-rs/` - this is the frontend user interface implementation, uses rust + yew compiles to wasm.
- `server/` - this is the backend server, interacts with the database and in the future runs various automation tasks and (in the future) machine learning models.
- `shared/` - shared backend operations - more or less reused modules between `server` and the two supporting command line tools below (`cli/` and `crawler/`)

Additionally, there's two supporting command line tools (`n2s` command line interface under `cli/` and `crawler` command line tool) which are mostly deprecated, except that the CLI is still needed on a first use to initialize the database table schemas.

- `cli/` - [[mostly deprecated except for initialization]] the command line tool. this is mostly no longer needed except to initialize the table schemas of the database (`n2s --reset --note ""`), but can also be used to test adding notes at the command line eg `n2s --note "this is a note" --tag "some_tag" --tag "another_tag`)
- `crawler/` - [[mostly deprecated]] for all notes consisting of urls, this crawls them, pulls html content into the database, but also takes screenshots, thumbnails, and runs ocr for a text representation of screenshots. This tool is also mostly deprecated in favor of running these operations synchronously upon adding a note instead of requiring users to run this process in batch on-demand.
- `electron/` - experimental Electron UI (not functioning yet).

There's also directories where artifacts are stored:

- `screenshots/` - screenshots captured by the headless browser
- `thumbnails/` - scaled down version of screenshots
- `ocr/` - tesseract ocr output of screenshots (in the future this might be derived from a higher-resolution image of the full site)

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
