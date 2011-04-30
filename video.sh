#!/bin/sh -e

cabal build

rm -fr /tmp/uvc_*.yuy2
./dist/build/test/test video +RTS -N3
ffplay -f rawvideo -pix_fmt yuyv422  -s 160x120 /tmp/uvc_160_120.yuy2
