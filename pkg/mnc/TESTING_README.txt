# $Header: /u/gcmpack/MITgcm/pkg/mnc/TESTING_README.txt,v 1.3 2007/10/08 17:00:17 jmc Exp $
# $Name:  $

The files here (the "MNC package") have an intentionally weak link to
MITgcm.  Since few FORTRAN common blocks are shared with the main
MITgcm code, many of the MNC files could be compiled independently.

Unlike the other MITgcm "packages", MNC includes a local Makefile and
various testing files (*.t, *.T) that can be used to create stand-
alone tests of parts of the MNC functionality.  Useful make targets
include:

  make
  make test
  make clean

which can be used to compile, run a test program, and/or clean up the
temporary files so that they will not conflict with normal MITgcm
builds.

