C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_cvars.F,v 1.8 2008/02/05 13:38:45 mlosch Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"

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
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif

C     Functions
      integer IFNBLNK, ILNBLNK

C     !INPUT PARAMETERS:
      character*(*) fname
      character*(*) cvname
      integer fid, did, bi,bj
      integer myThid
CEOP

C     !LOCAL VARIABLES:
      integer i, vid, nnf, nnl, doit, err
      integer nids, cv_did(1), xtmin,ytmin
      character*(MAX_LEN_MBUF) msgbuf
      integer cv_start(1), cv_count(1)
      _RS rtmp(sNx + 2*OLx + sNy + 2*OLy + Nr)

      nnf = IFNBLNK(cvname)
      nnl = ILNBLNK(cvname)

      xtmin = 0
      ytmin = 0
#ifdef ALLOW_EXCH2
      xtmin = exch2_tbasex(W2_myTileList(bi))
      ytmin = exch2_tbasey(W2_myTileList(bi))
#endif
      doit = 1
      nids = 1
      cv_did(1)= did

C     Check all the coordinate variables that we know about
      IF (cvname(nnf:nnl) .EQ. 'X') THEN

        cv_start(1) = 1
        cv_count(1) = sNx
#ifdef ALLOW_EXCH2
        DO i = cv_start(1),cv_count(1)
         rtmp(i) = xtmin + i
        ENDDO
#else
        IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = xtmin + i
         ENDDO
        ELSE
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = xC(i,1,bi,bj)
         ENDDO
        ENDIF
#endif

      ELSEIF (cvname(nnf:nnl) .EQ. 'Xp1') THEN

        cv_start(1) = 1
        cv_count(1) = sNx + 1
#ifdef ALLOW_EXCH2
        DO i = cv_start(1),cv_count(1)
         rtmp(i) = xtmin + i
        ENDDO
#else
        IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = xtmin + i
         ENDDO
        ELSE
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = xG(i,1,bi,bj)
         ENDDO
        ENDIF
#endif

      ELSEIF (cvname(nnf:nnl) .EQ. 'Xwh') THEN

        cv_start(1) = 1
        cv_count(1) = sNx + 2*OLx
#ifdef ALLOW_EXCH2
        DO i = cv_start(1),cv_count(1)
         rtmp(i) = xtmin + i
        ENDDO
#else
        IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = xtmin - OLx + i
         ENDDO
        ELSE
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = xC(i,1,bi,bj)
         ENDDO
        ENDIF
#endif
        
      ELSEIF (cvname(nnf:nnl) .EQ. 'Y') THEN

        cv_start(1) = 1
        cv_count(1) = sNy
#ifdef ALLOW_EXCH2
        DO i = cv_start(1),cv_count(1)
         rtmp(i) = ytmin + i
        ENDDO
#else
        IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = ytmin + i
         ENDDO
        ELSE
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = yC(1,i,bi,bj)
         ENDDO
        ENDIF
#endif

      ELSEIF (cvname(nnf:nnl) .EQ. 'Yp1') THEN

        cv_start(1) = 1
        cv_count(1) = sNy + 1
#ifdef ALLOW_EXCH2
        DO i = cv_start(1),cv_count(1)
         rtmp(i) = ytmin + i
        ENDDO
#else
        IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = ytmin + i
         ENDDO
        ELSE
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = yG(1,i,bi,bj)
         ENDDO
        ENDIF
#endif

      ELSEIF (cvname(nnf:nnl) .EQ. 'Ywh') THEN

        cv_start(1) = 1
        cv_count(1) = sNy + 2*OLy
#ifdef ALLOW_EXCH2
        DO i = cv_start(1),cv_count(1)
         rtmp(i) = ytmin + i
        ENDDO
#else
        IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = ytmin - OLy + i
         ENDDO
        ELSE
         DO i = cv_start(1),cv_count(1)
          rtmp(i) = yC(1,i-OLy,bi,bj)
         ENDDO
        ENDIF
#endif

      ELSEIF (cvname(nnf:nnl) .EQ. 'Z') THEN

        cv_start(1) = 1
        cv_count(1) = Nr
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rC(i)
        ENDDO

      ELSEIF (cvname(nnf:nnl) .EQ. 'Zp1') THEN

        cv_start(1) = 1
        cv_count(1) = Nr + 1
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rF(i)
        ENDDO

      ELSEIF (cvname(nnf:nnl) .EQ. 'Zu') THEN

        cv_start(1) = 1
        cv_count(1) = Nr
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rF(i + 1)
        ENDDO

      ELSEIF (cvname(nnf:nnl) .EQ. 'Zl') THEN

        cv_start(1) = 1
        cv_count(1) = Nr
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rF(i)
        ENDDO

      ELSEIF (cvname(nnf:nnl) .EQ. 'Zm1') THEN

        cv_start(1) = 1
        cv_count(1) = Nr - 1
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rF(i + 1)
        ENDDO

      ELSE

        doit = 0

      ENDIF

      IF ( doit .EQ. 1 ) THEN

        CALL MNC_FILE_REDEF(fname, myThid)
        err = NF_DEF_VAR(fid, cvname, NF_DOUBLE, 
     &       nids, cv_did, vid)
        i = ILNBLNK( fname )
        write(msgbuf,'(5a)') 'defining coordinate variable ''', 
     &       cvname(nnf:nnl), ''' in file ''', fname(1:i), ''''
        CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
        CALL MNC_FILE_ENDDEF(fname, myThid)
        err = NF_PUT_VARA_DOUBLE(fid, vid, 
     &       cv_start, cv_count, rtmp)
        write(msgbuf,'(5a)') 'writing coordinate variable ''', 
     &       cvname(nnf:nnl), ''' in file ''', fname(1:i), ''''
        CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
        
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

