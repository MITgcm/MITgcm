C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_test_001.T,v 1.4 2004/01/07 19:50:52 edhill Exp $
C $Name:  $

C     Simple test program for MNC

      PROGRAM mnc_test_001

      implicit none
#include "MNC_OPTIONS.h"
#include "EEPARAMS.h"

C     Functions
      integer ILNBLNK

C     Local Variables
      integer MAX_STR, MAX_NAMES, MAX_DIM, MAX_STOR
      PARAMETER ( MAX_STR   =  100 )
      PARAMETER ( MAX_NAMES =   10 )
      PARAMETER ( MAX_DIM   =   10 )
      PARAMETER ( MAX_STOR  = 1000 )
      integer ndim, dims(MAX_DIM)
      integer myThid, i,j,k,l,ii, n, ind, fid
      character*(MAX_LEN_MBUF) msgbuf
      character*(MAX_STR) blank, fname, gname, vname, units
      character*(MAX_STR) atname, atval
      character*(MAX_STR) dnames(MAX_NAMES)
      REAL*8 fillval
      REAL*8 var(MAX_STOR)

      print*, '=== Starting Program:  mnc_test_001 ==='

      print*, ' Initalizing variables ...'
      DO i = 1,MAX_STR
        blank(i:i) = ' '
      ENDDO
      DO i = 1,MAX_NAMES
        dnames(i)(1:MAX_STR) = blank(1:MAX_STR)
      ENDDO
      fname(1:MAX_STR) = blank(1:MAX_STR)
      gname(1:MAX_STR) = blank(1:MAX_STR)
      vname(1:MAX_STR) = blank(1:MAX_STR)
      units(1:MAX_STR) = blank(1:MAX_STR)
      atname(1:MAX_STR) = blank(1:MAX_STR)
      atval(1:MAX_STR) = blank(1:MAX_STR)
      fname(1:12) = 'test_001.nc '
      ndim = 4
      dims(1) =  7
      dims(2) =  4
      dims(3) =  2
      dims(4) = -1 ! -1 
      dnames(1)(1:1) = 'X'
      dnames(2)(1:1) = 'Y'
      dnames(3)(1:1) = 'Z'
      dnames(4)(1:1) = 'T '
      myThid = 1
C     In FORTRAN, the first variable cycles most quickly
      DO ii = 1,MAX_STOR
        var(ii) = 0.21
      ENDDO
      ii = 0
      DO l = 1,dims(3)
        DO k = 1,dims(3)
          DO j = 1,dims(2)
            DO i = 1,dims(1)
              ii = ii + 1
              var(ii) = ((i*100 + j)*100 + k)*100 + l
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      print*, ' Calling MNC_INIT() ...'
      CALL MNC_INIT(myThid)

      print*, ' Calling MNC_DIM_INIT() ...'
      DO i = 1,ndim
        CALL MNC_DIM_INIT(myThid, dnames(i), dnames(i), dims(i))
        print*, '   ... created dimension: ', i
      ENDDO

      print*, ' Calling MNC_FILE_CREATE() ...'
      CALL MNC_FILE_CREATE(myThid, fname)

      print*, ' Calling MNC_GRID_INIT() ...'
      gname(1:8) = 'XY_Grid '
      CALL MNC_GRID_INIT(myThid, fname, gname, ndim, dnames)

      print*, ' Calling MNC_VAR_INIT_DBL() ...'
      vname(1:11) = 'temperature'
      units(1:3) = 'm/s'
      fillval = -9999.0
      CALL MNC_VAR_INIT_DBL(myThid, fname, gname, vname, fillval)

      print*, ' Calling MNC_VAR_ADD_ATTR_STR() ...'
      atname(1:5) = 'units'
      atval(1:4) = 'cm/s'
      CALL MNC_VAR_ADD_ATTR_STR(myThid, fname, vname, atname, atval)

      print*, ' Calling MNC_VAR_WRITE_DBL() ...'
      CALL MNC_VAR_WRITE_DBL(myThid, fname, vname, var)
      print*, ' Calling MNC_VAR_WRITE_DBL() ...'
      CALL MNC_VAR_WRITE_DBL(myThid, fname, vname, var)

      print*, ' Calling MNC_FILE_CLOSE() ...'
      CALL MNC_FILE_CLOSE(myThid, fname)

      print*, '=== Ending Program:  mnc_test_001 ==='

      END

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
