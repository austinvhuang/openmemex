#!/usr/bin/env bash
set -x # enable xtrace
cd /app

HASKTORCH_LIB_PATH="$(pwd)/libtorch/lib/"
LD_LIBRARY_PATH=$HASKTORCH_LIB_PATH:$(pwd)/deps/tokenizers:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH

stack run server

