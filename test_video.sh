#!/bin/sh -e

# Clean
rm -fr /tmp/uvc_*.yuy2

# Run haskell
./dist/build/test/test saveraw +RTS -N3


./read_video.sh /tmp/uvc_*.yuy2
