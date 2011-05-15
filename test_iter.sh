#!/bin/sh -e

BIN=dist/build/iteratee-test/iteratee-test

if ! [ -x ${BIN} ];then
	echo 2>&1 "Iteratee-test binary not found !"
	echo 2>&1 "Have you compiled with the -fIteratee flag ?"
	exit 1
fi

# Clean
rm -fr /tmp/uvc_*.yuy2

# Run haskell
${BIN} saveraw +RTS -N3


./read_video.sh /tmp/uvc_*.yuy2
