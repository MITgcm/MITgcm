C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_cvars.F,v 1.1 2004/12/17 21:28:25 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C CBOP 1
C C     !ROUTINE: MNC_CW_DEF_CVAR
      
C C     !INTERFACE:
C       SUBROUTINE MNC_CW_DEF_CVAR( 
C      I     cvname, 
C      I     cvnum,
C      I     cvdat, 
C      I     myThid )

C C     !DESCRIPTION:
C C     Store data for a CF-convention coordinate variable (a vector) for
C C     the given dimension name.

C C     !USES:
C       implicit none
C #include "mnc_common.h"
C #include "EEPARAMS.h"

C C     !INPUT PARAMETERS:
C       character*(*) cvname
C       _RL cvdat(*)
C       integer cvnum, myThid
C CEOP

C C     !LOCAL VARIABLES:
C       integer i, imx, ind, nnf,nnl
C       character*(MAX_LEN_MBUF) msgbuf

C C     Functions
C       integer IFNBLNK, ILNBLNK

C       nnf = IFNBLNK(cvname)
C       nnl = ILNBLNK(cvname)

C C     Check that this name is not already defined
C       CALL MNC_GET_IND(MNC_MAX_ID, cvname, mnc_cw_cvnm, ind, myThid)
C       IF (ind .GT. 0) THEN
C         write(msgbuf,'(3a)') 'MNC_CW_DEF_CVAR ERROR: ''', 
C      &       cvname(nnf:nnl), ''' is already defined'
C         CALL print_error(msgbuf, mythid)
C         stop 'ABNORMAL END: S/R MNC_CW_DEF_CVAR'
C       ENDIF
C       CALL MNC_GET_NEXT_EMPTY_IND(MNC_MAX_ID, mnc_cw_cvnm, 
C      &     ind, myThid)

C       imx = 0
C       DO i = 1,MNC_MAX_ID
C         IF (mnc_cw_cvnm(i)(1:1) .NE. ' ') THEN
C           imx = max(imx, mnc_cw_cvse(2,i))
C         ENDIF
C       ENDDO

C       IF ((MNC_CW_CVDAT - imx - 1) .LT. cvnum) THEN
C         write(msgbuf,'(3a)') 'MNC_CW_DEF_CVAR ERROR: out of space',
C      &       '--please increase size of MNC_CW_CVDAT in the file ',
C      &       '''mnc_common.h'''
C         CALL print_error(msgbuf, mythid)
C         stop 'ABNORMAL END: S/R MNC_CW_DEF_CVAR'
C       ENDIF

C       DO i = 1,cvnum
C         mnc_cw_cvdt(imx+i) = cvdat(i)
C       ENDDO
C       mnc_cw_cvse(1,ind) = imx + 1
C       mnc_cw_cvse(2,ind) = imx + cvnum

C       RETURN
C       END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_CW_WRITE_CVAR
      
C     !INTERFACE:
      SUBROUTINE MNC_CW_WRITE_CVAR( 
     I     fname, 
     I     cvname, 
     I     fid, 
     I     did, 
     I     bi, bj, 
     I     myThid )

C     !DESCRIPTION:
C     Write a CF-convention coordinate variable (a vector).

C     !USES:
      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT PARAMETERS:
      character*(*) fname
      character*(*) cvname
      integer fid, did, bi,bj
      integer myThid
CEOP

C     !LOCAL VARIABLES:
      integer i, vid, nnf,nnl, doit, err
      integer nids, cv_did(1)
      character*(MAX_LEN_MBUF) msgbuf
      integer cv_start(1), cv_count(1)
      _RS rtmp(sNx * sNy)

C     Functions
      integer IFNBLNK, ILNBLNK

      nnf = IFNBLNK(cvname)
      nnl = ILNBLNK(cvname)

      doit = 0
      nids = 1
      cv_did(1)= did

C     Check all the coordinate variables that we know about
      IF (cvname(nnf:nnl) .EQ. 'X') THEN

        cv_start(1) = 1
        cv_count(1) = sNx
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = xC(i,1,bi,bj)
        ENDDO
        doit = 1

      ELSEIF (cvname(nnf:nnl) .EQ. 'Xp1') THEN

        cv_start(1) = 1
        cv_count(1) = sNx + 1
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = xG(i,1,bi,bj)
        ENDDO
        doit = 1

      ELSEIF (cvname(nnf:nnl) .EQ. 'Y') THEN

        cv_start(1) = 1
        cv_count(1) = sNy
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = yC(1,i,bi,bj)
        ENDDO
        doit = 1

      ELSEIF (cvname(nnf:nnl) .EQ. 'Yp1') THEN

        cv_start(1) = 1
        cv_count(1) = sNy + 1
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = yG(1,i,bi,bj)
        ENDDO
        doit = 1

      ELSEIF (cvname(nnf:nnl) .EQ. 'Z') THEN

        cv_start(1) = 1
        cv_count(1) = Nr
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rC(i)
        ENDDO
        doit = 1

      ELSEIF (cvname(nnf:nnl) .EQ. 'Zp1') THEN

        cv_start(1) = 1
        cv_count(1) = Nr + 1
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rF(i)
        ENDDO
        doit = 1

      ENDIF

      IF ( doit .EQ. 1 ) THEN

        CALL MNC_FILE_REDEF(fname, myThid)
        err = NF_DEF_VAR(fid, cvname, NF_DOUBLE, 
     &       nids, cv_did, vid)
        write(msgbuf,'(5a)') 'defining coordinate variable ''', 
     &       cvname(nnf:nnl), ''' in file ''', fname, ''''
        CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
        CALL MNC_FILE_ENDDEF(fname, myThid)
        err = NF_PUT_VARA_DOUBLE(fid, vid, 
     &       cv_start, cv_count, rtmp)
        write(msgbuf,'(5a)') 'writing coordinate variable ''', 
     &       cvname(nnf:nnl), ''' in file ''', fname, ''''
        CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
        
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

