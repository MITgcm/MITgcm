

The files here (the "MNC package") have an intentionally weak link to
MITgcm.  Since no FORTRAN common blocks are shared with the main
MITgcm code, the MNC files could be built as a separate library.

Unlike the other MITgcm "packages", MNC includes a local Makefile and
various testing files (*.t, *.T) that can be used to create stand-
alone tests of the MNC functionality.  Useful make targets include:

  make
  make test
  make clean

which can be used to compile, run a test program, and/or clean up the
temporary files so that they will not conflict with normal MITgcm
builds.

