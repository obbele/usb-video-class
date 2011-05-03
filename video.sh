#!/bin/sh -e

# Clean
rm -fr /tmp/uvc_*.yuy2

# Run haskell
./dist/build/test/test saveraw +RTS -N3


SIZE=`ls -1 /tmp/uvc_*.yuy2 | grep -oE '[0-9]+x[0-9]+'`
WIDTH=`echo $SIZE | cut -d'x' -f1`
HEIGHT=`echo $SIZE | cut -d'x' -f2`
FILE=`echo /tmp/uvc_*.yuy2`

# Try to use gstreamer or ffplay
gst-launch \
	filesrc location="${FILE}" \
	! videoparse format=3 width=$WIDTH height=$HEIGHT framerate=10/1 \
	! autovideosink \
 || ffplay -f rawvideo -pix_fmt yuyv422  -s ${SIZE} "${FILE}"
