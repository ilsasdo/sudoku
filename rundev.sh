#!/usr/bin/env bash

elm-live src/Main.elm --host=127.0.0.1 --dir=dist -- --output=dist/build.js --debug
