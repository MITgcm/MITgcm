C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_test_001.T,v 1.11 2004/03/19 03:28:37 edhill Exp $
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
      character*(MAX_STR) blank, fname, gname1,gname2, vname1,vname2
      character*(MAX_STR) atname, atval, units
      character*(MAX_STR) dnames(MAX_NAMES)
      REAL*8 fillval
      REAL*8 var(MAX_STOR), tvar(MAX_STOR)

      print*, '=== Starting Program:  mnc_test_001 ==='

      print*, ' Initalizing variables ...'
      DO i = 1,MAX_STR
        blank(i:i) = ' '
      ENDDO
      DO i = 1,MAX_NAMES
        dnames(i)(1:MAX_STR) = blank(1:MAX_STR)
      ENDDO
      fname(1:MAX_STR) = blank(1:MAX_STR)
      gname1(1:MAX_STR) = blank(1:MAX_STR)
      gname2(1:MAX_STR) = blank(1:MAX_STR)
      vname1(1:MAX_STR) = blank(1:MAX_STR)
      vname2(1:MAX_STR) = blank(1:MAX_STR)
      units(1:MAX_STR) = blank(1:MAX_STR)
      atname(1:MAX_STR) = blank(1:MAX_STR)
      atval(1:MAX_STR) = blank(1:MAX_STR)
      fname(1:12) = 'test_001.nc '
      ndim = 4
      dims(1) =  7
      dims(2) =  4
      dims(3) =  2
      dims(4) = -1
      dnames(1)(1:1) = 'X'
      dnames(2)(1:1) = 'Y'
      dnames(3)(1:1) = 'Z'
      dnames(4)(1:1) = 'T'
      myThid = 1
C     In FORTRAN, the first variable cycles most quickly
      DO ii = 1,MAX_STOR
        var(ii) = 0.21
        tvar(ii) = 0.23
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

      print*, ' Calling MNC_FILE_CREATE() ...'
      CALL MNC_FILE_CREATE(fname, myThid)

      print*, ' Calling MNC_DIM_INIT() ...'
      DO i = 1,ndim
        CALL MNC_DIM_INIT(fname, dnames(i),dims(i), myThid)
        ii = ILNBLNK(dnames(i))
        print*, '   ... created dimension: ', dnames(i)(1:ii)
      ENDDO

      print*, ' Calling MNC_GRID_INIT() ...'
      gname1(1:10) = 'xyzt      '
      CALL MNC_GRID_INIT(fname, gname1, ndim, dnames, myThid)
      gname2(1:10) = 't         '
      CALL MNC_GRID_INIT(fname, gname2, 1, dnames(4), myThid)

      print*, ' Calling MNC_VAR_INIT_DBL() ...'
      vname1(1:4) = 'time'
      units(1:19) = 'days since 1970-1-1'
      CALL MNC_VAR_INIT_DBL(fname, gname2, vname1, myThid)
      vname2(1:4) = 'temp'
      units(1:19) = 'deg C              '
      CALL MNC_VAR_INIT_DBL(fname, gname1, vname2, myThid)

      print*, ' Calling MNC_VAR_ADD_ATTR_STR() ...'
      atname(1:9) = 'grid_type'
      CALL MNC_VAR_ADD_ATTR_INT(fname,vname1, atname, 1, 1, myThid)
      atname(1:11) = 'valid_range'
      tvar(1) = -23.5
      tvar(2) = 176.75
      CALL MNC_VAR_ADD_ATTR_DBL(fname,vname2, atname, 2, tvar, myThid)

      print*, ' Calling MNC_VAR_WRITE_DBL() ...'
      CALL MNC_VAR_WRITE_DBL(fname, vname1, var, myThid)
      print*, ' Calling MNC_VAR_WRITE_DBL() ...'
      CALL MNC_VAR_WRITE_DBL(fname, vname2, var, myThid)

      CALL MNC_DUMP(myThid)

      print*, ' Calling MNC_FILE_CLOSE() ...'
      CALL MNC_FILE_CLOSE(fname, myThid)

      CALL MNC_DUMP(myThid)

      print*, '=========================================='

      print*, ' Calling MNC_FILE_READALL() ...'
      CALL MNC_FILE_READALL(fname, myThid)

      CALL MNC_DUMP(myThid)

      print*, '=== Ending Program:  mnc_test_001 ==='

      END

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
