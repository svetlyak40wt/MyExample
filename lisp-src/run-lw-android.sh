#!/bin/sh

# -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/android:run-lw-android.sh,v 1.4.2.4 2017/10/27 14:00:30 martin Exp $" -*-

# This script runs the LispWorks for Android Runtime image under QEMU.

# These variables can be changed if you have installed the components
# in other directories.
QEMUROOT="$HOME/projects/lisp/lw-android/qemu"
LWANDROIDROOT="$HOME/projects/lisp/lw-android"
VERSION=7-1-0

LWIMAGE="$LWANDROIDROOT/lispworks-$VERSION-arm-linux-android"

# Run the Android image using QEMU.
qemubin=qemu-arm
if test `uname` = "Darwin"; then
  export DYLD_LIBRARY_PATH="$QEMUROOT/lib"
  qemubin="$QEMUROOT/bin/$qemubin"
else
  case `uname -m` in
  arm*|aarch64*)
    exec "$LWIMAGE" "$@"
    ;;
  esac
fi
	
"$qemubin" -L "$QEMUROOT/arm-linux-libs" "$LWIMAGE" "$@"
