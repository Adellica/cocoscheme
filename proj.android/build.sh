#!/bin/bash

csc -s -t -c++ ../scm/cocoscheme.scm -o ./jni/cocoscheme.cpp -include-path ../scm && NDK_ROOT=/home/klm/opt/android-ndk-r8/ ./build_native.sh && ant clean debug && adb uninstall com.adellica.cocoscheme && adb install -r bin/cocoscheme-debug.apk
