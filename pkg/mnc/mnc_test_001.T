C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_test_001.T,v 1.3 2004/01/07 07:29:13 edhill Exp $
C $Name:  $

C     Simple test program for MNC

      PROGRAM mnc_test_001

      implicit none
#include "MNC_OPTIONS.h"
#include "EEPARAMS.h"

C     Functions
      integer ILNBLNK

C     Local Variables
      integer MAX_STR, MAX_NAMES
      PARAMETER ( MAX_STR = 100 )
      PARAMETER ( MAX_NAMES = 10 )
      integer myThid, i, n, ind, fid
      character*(MAX_LEN_MBUF) msgbuf
      character*(MAX_STR) blank, fname, gname, vname, units
      character*(MAX_STR) atname, atval
      character*(MAX_STR) names(MAX_NAMES)
      REAL*8 fillval

      print*, '=== Starting Program:  mnc_test_001 ==='
      DO i = 1,MAX_STR
        blank(i:i) = ' '
      ENDDO
      DO i = 1,MAX_NAMES
        names(i)(1:MAX_STR) = blank(1:MAX_STR)
      ENDDO
      fname(1:MAX_STR) = blank(1:MAX_STR)
      gname(1:MAX_STR) = blank(1:MAX_STR)
      vname(1:MAX_STR) = blank(1:MAX_STR)
      units(1:MAX_STR) = blank(1:MAX_STR)
      atname(1:MAX_STR) = blank(1:MAX_STR)
      atval(1:MAX_STR) = blank(1:MAX_STR)
      fname(1:12) = 'test_001.nc '

      myThid = 1
      print*, ' Calling MNC_INIT() ...'
      CALL MNC_INIT(myThid)

      print*, ' Calling MNC_DIM_INIT() ...'
      CALL MNC_DIM_INIT(myThid, 'X ', 'X ', 11)
      print*, '   ... created 1 dimension'
      CALL MNC_DIM_INIT(myThid, 'Y ', 'Y ', 4)
      print*, '   ... created 2 dimensions'
      CALL MNC_DIM_INIT(myThid, 'Z ', 'Z ', 2)
      print*, '   ... created 3 dimensions'
      CALL MNC_DIM_INIT(myThid, 'T ', 'T ', -1)
      print*, '   ... created 4 dimensions'

      print*, ' Calling MNC_FILE_CREATE() ...'
      CALL MNC_FILE_CREATE(myThid, fname)

      print*, ' Calling MNC_GRID_INIT() ...'
      gname(1:8) = 'XY_Grid '
      n = 4
      names(1)(1:2) = 'X'
      names(2)(1:2) = 'Y'
      names(3)(1:2) = 'Z'
      names(4)(1:2) = 'T'
      CALL MNC_GRID_INIT(myThid, fname, gname, n, names)

      print*, ' Calling MNC_VAR_INIT_DBL() ...'
      vname(1:11) = 'temperature'
      units(1:3) = 'm/s'
      fillval = -9999.0
      CALL MNC_VAR_INIT_DBL(myThid, fname, gname, vname, fillval)

      print*, ' Calling MNC_VAR_ADD_ATTR_STR() ...'
      atname(1:5) = 'units'
      atval(1:4) = 'cm/s'
      CALL MNC_VAR_ADD_ATTR_STR(myThid, fname, vname, atname, atval)

      print*, ' Calling MNC_FILE_CLOSE() ...'
      CALL MNC_FILE_CLOSE(myThid, fname)

      print*, '=== Ending Program:  mnc_test_001 ==='

      END

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
