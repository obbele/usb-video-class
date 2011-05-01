#!/bin/sh -e

rm -fr /tmp/uvc_*.yuy2
./dist/build/test/test video +RTS -N3
SIZE=`ls -1 /tmp/uvc_*.yuy2 | grep -oE '[0-9]+x[0-9]+'`
ffplay -f rawvideo -pix_fmt yuyv422  -s ${SIZE} /tmp/uvc_*.yuy2
