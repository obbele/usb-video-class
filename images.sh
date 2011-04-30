#!/bin/sh -e

cabal build

rm -fr /tmp/uvc_*.bmp
./dist/build/test/test images +RTS -N3
feh -Z -g640x480 /tmp/uvc_*bmp
