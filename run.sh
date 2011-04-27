#!/bin/sh -e

cabal build
./dist/build/test/test
