Setup
=====

This project requires LispWorks Android runtime to be installed
into `$HOME/projects/lisp/lw-android`.

If you have it in other directory, edit `lisp-src/run-lw-android.sh` file.

If you don't have LW for Android, request an evaluation license from lisp-sales@lispworks.com.


To build Lisp Part
==================

Run:

```
lisp-src/run-lw-android.sh -build lisp-src/deliver-my-example.lisp
```

To build Android part
=====================

Open this project in Android Studio and push "Run" button.
