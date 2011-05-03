#!/bin/sh -e

rm -fr /tmp/uvc_*.bmp
./dist/build/test/test savebmp +RTS -N3
feh -Z -g1024x768 /tmp/uvc_*bmp
