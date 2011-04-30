#!/bin/sh -e

cabal build

rm -fr /tmp/uvc_*.bmp
./dist/build/test/test images +RTS -N2
feh -Z -g640x480 /tmp/uvc_*bmp
