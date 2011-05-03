#!/bin/sh -e

if [ x"$1" = "x" ];then
	echo 2>&1 "Usage: ./read_video.sh filename.yuy2"
	exit 1
fi

FILE="$1"
SIZE=`echo $FILE | grep -oE '[0-9]+x[0-9]+'`
WIDTH=`echo $SIZE | cut -d'x' -f1`
HEIGHT=`echo $SIZE | cut -d'x' -f2`

# Try to use gstreamer or ffplay
gst-launch \
	filesrc location="${FILE}" \
	! videoparse format=3 width=$WIDTH height=$HEIGHT framerate=10/1 \
	! autovideosink \
 || ffplay -f rawvideo -pix_fmt yuyv422  -s ${SIZE} "${FILE}"
