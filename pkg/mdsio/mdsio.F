C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio.F,v 1.1 2001/03/06 15:28:53 adcroft Exp $
C $Name:  $

      SUBROUTINE MDSIO
C
C     Blank routine to disable the original mdsio.F file when used
C     with the -enable=mdsio option of genmake
C
C     This is *NOT* the normal way of doing things but is a temporary
C     measure to facilitate testing of the new package without breaking
C     applications.  To use the original mdsio.F use:
C                genmake -disable=mdsio
C
      RETURN
      END
