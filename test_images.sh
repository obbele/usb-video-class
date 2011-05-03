#!/bin/sh -e

rm -fr /tmp/uvc_*.bmp
./dist/build/test_bmp/test_bmp +RTS -N3
feh -Z -g1024x768 /tmp/uvc_*bmp
