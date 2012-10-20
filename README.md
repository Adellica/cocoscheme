

## Cocos2Dx with Chicken Scheme

This is the source code for the [cocoscheme](https://play.google.com/store/apps/details?id=com.adellica.cocoscheme) demo on Google Play.

It has, unfortunately, not reached a presentable state yet. The build-process is combersome and the code is untidy. There are even hard-coded paths... It's published nevertheless because people have asked for it!

### Dependencies

* Chicken Scheme
* The newest version of [chickmunk](https://github.com/kristianlm/chickmunk)
* The newest version of [this](https://github.com/kristianlm/chicken-bind) chicken-bind.
* [chicken-android](https://github.com/kristianlm/chicken-android) to build
* The Android SDK and NDK

### Rough layout

The native Cocos2Dx part of the project hasn't changed much from the HelloCpp sample found in [Cocos2Dx version 2.0.3](http://www.cocos2d-x.org/news/74). A few external functions have been added and are called with the 'update' and `draw` callbacks. These are implemented in Scheme with `(define-external ...`.

These hand the control over to Chicken which can handle things from there, including the network REPL and it's commands. There is a `*update*` and a `*draw*` Scheme global which can be redefined at runtime.

Note that Chipmunk, as included by Cocos2Dx is ignored as we are going for [chickmunk](https://github.com/kristianlm/chickmunk) instead. At some point, we wish to have a native 'SpaceManager' (see todo), but otherwise keep the projects as isolated as possible.

The current layout will only build for Android. The Linux build is broken, but should be easy to fix. To build, we're using the [chicken-android](https://github.com/kristianlm/chicken-android) project.

Check out `scm/CCNode.bind.hpp` for the native functions available. 

All APIs are subject to change, of course!

### Versioning

Tags beginning with `gp` are the versions of official Google Play releases (currently only 1.0).