#!/bin/sh -e

rm -fr /tmp/uvc_*.bmp
./dist/build/test/test images +RTS -N3
feh -Z -g1024x768 /tmp/uvc_*bmp
