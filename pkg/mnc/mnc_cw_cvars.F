C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_cvars.F,v 1.16 2011/05/23 01:08:22 jmc Exp $
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
#include "MNC_COMMON.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif
#include "netcdf.inc"

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
C     variables for text attributes
      integer MAX_LEN_NAME, ia
      PARAMETER ( MAX_LEN_NAME = 128 )
      character*(MAX_LEN_NAME) units, long_name, positive

      DO i=1,MAX_LEN_NAME
         units(i:i)     = ' '
         long_name(i:i) = ' '
         positive(i:i)  = ' '
      ENDDO

      nnf = IFNBLNK(cvname)
      nnl = ILNBLNK(cvname)

      xtmin = 0
      ytmin = 0
#ifdef ALLOW_EXCH2
      xtmin = exch2_tbasex(W2_myTileList(bi,bj))
      ytmin = exch2_tbasey(W2_myTileList(bi,bj))
#else
      IF ( .NOT. useCubedSphereExchange ) THEN
C     make sure for a non-cubed-sphere curvi-linear grid,
C     that the X/Y coordinate variables are monotonous
C     bi+(myXGlobalLo-1)/sNx is the i-tile number
C     bj+(myYGlobalLo-1)/sNy is the j-tile number
       xtmin = sNx * ( bi+(myXGlobalLo-1)/sNx - 1 )
       ytmin = sNy * ( bj+(myYGlobalLo-1)/sNy - 1 )
      ENDIF
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
        IF ( usingCartesianGrid ) THEN
         long_name = 'X-coordinate of cell center'
         units     = 'meters'
        ELSEIF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         long_name = 'i-index of cell center'
         units     = 'none'
        ELSEIF ( usingSphericalPolarGrid ) THEN
         long_name = 'longitude of cell center'
         units     = 'degrees_east'
        ELSEIF ( usingCylindricalGrid ) THEN
         long_name = 'polar angle coordinate of cell center'
         units     = 'degrees'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown horizontal grid!'
        ENDIF

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
        IF ( usingCartesianGrid ) THEN
         long_name = 'X-Coordinate of cell corner'
         units     = 'meters'
        ELSEIF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         long_name = 'i-index of cell corner'
         units     = 'none'
        ELSEIF ( usingSphericalPolarGrid ) THEN
         long_name = 'longitude of cell corner'
         units     = 'degrees_east'
        ELSEIF ( usingCylindricalGrid ) THEN
         long_name = 'polar angle  of cell corner'
         units     = 'degrees'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown horizontal grid!'
        ENDIF

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
CML????          rtmp(i) = xC(i-Olx,1,bi,bj)
         ENDDO
        ENDIF
#endif
        IF ( usingCartesianGrid ) THEN
         long_name = 'X-Coordinate of cell center including overlaps'
         units     = 'meters'
        ELSEIF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         long_name = 'i-index of cell center including overlaps'
         units     = 'none'
        ELSEIF ( usingSphericalPolarGrid ) THEN
         long_name = 'longitude of cell center including overlaps'
         units     = 'degrees_east'
        ELSEIF ( usingCylindricalGrid ) THEN
         long_name =
     &        'polar angle coordinate of cell center including overlaps'
         units     = 'degrees'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown horizontal grid!'
        ENDIF

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
        IF ( usingCartesianGrid ) THEN
         long_name = 'Y-Coordinate of cell center'
         units     = 'meters'
        ELSEIF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         long_name = 'j-index of cell center'
         units     = 'none'
        ELSEIF ( usingSphericalPolarGrid ) THEN
         long_name = 'latitude of cell center'
         units     = 'degrees_north'
        ELSEIF ( usingCylindricalGrid ) THEN
         long_name = 'radial coordinate of cell center'
         units     = 'meters'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown horizontal grid!'
        ENDIF

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
        IF ( usingCartesianGrid ) THEN
         long_name = 'Y-Coordinate of cell corner'
         units     = 'meters'
        ELSEIF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         long_name = 'j-index of cell corner'
         units     = 'none'
        ELSEIF ( usingSphericalPolarGrid ) THEN
         long_name = 'latitude of cell corner'
         units     = 'degrees_north'
        ELSEIF ( usingCylindricalGrid ) THEN
         long_name = 'radial coordinate of cell corner'
         units     = 'meters'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown horizontal grid!'
        ENDIF

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
        IF ( usingCartesianGrid ) THEN
         long_name = 'Y-Coordinate of cell center including overlaps'
         units     = 'meters'
        ELSEIF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         long_name = 'j-index of cell center including overlaps'
         units     = 'none'
        ELSEIF ( usingSphericalPolarGrid ) THEN
         long_name = 'latitude of cell center including overlaps'
         units     = 'degrees_north'
        ELSEIF ( usingCylindricalGrid ) THEN
         long_name =
     &        'radial coordinate of cell center including overlaps'
         units     = 'meters'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown horizontal grid!'
        ENDIF

      ELSEIF (cvname(nnf:nnl) .EQ. 'Z') THEN

        cv_start(1) = 1
        cv_count(1) = Nr
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rC(i)
        ENDDO
C
        long_name = 'vertical coordinate of cell center'
        IF ( usingZCoords ) THEN
         units     = 'meters'
         positive  = 'up'
        ELSEIF ( usingPCoords ) THEN
         units     = 'pascal'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown vertical grid!'
        ENDIF

      ELSEIF (cvname(nnf:nnl) .EQ. 'Zp1') THEN

        cv_start(1) = 1
        cv_count(1) = Nr + 1
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rF(i)
        ENDDO
C
        long_name = 'vertical coordinate of cell interface'
        IF ( usingZCoords ) THEN
         units     = 'meters'
         positive  = 'up'
        ELSEIF ( usingPCoords ) THEN
         units     = 'pascal'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown vertical grid!'
        ENDIF

      ELSEIF (cvname(nnf:nnl) .EQ. 'Zu') THEN

        cv_start(1) = 1
        cv_count(1) = Nr
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rF(i + 1)
        ENDDO
C
        IF ( usingZCoords ) THEN
         long_name = 'vertical coordinate of lower cell interface'
         units     = 'meters'
         positive  = 'up'
        ELSEIF ( usingPCoords ) THEN
         long_name = 'vertical coordinate of upper cell interface'
         units     = 'pascal'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown vertical grid!'
        ENDIF

      ELSEIF (cvname(nnf:nnl) .EQ. 'Zl') THEN

        cv_start(1) = 1
        cv_count(1) = Nr
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rF(i)
        ENDDO
C
        IF ( usingZCoords ) THEN
         long_name = 'vertical coordinate of upper cell interface'
         units     = 'meters'
         positive  = 'up'
        ELSEIF ( usingPCoords ) THEN
         long_name = 'vertical coordinate of lower cell interface'
         units     = 'pascal'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown vertical grid!'
        ENDIF

      ELSEIF (cvname(nnf:nnl) .EQ. 'Zm1') THEN

        cv_start(1) = 1
        cv_count(1) = Nr - 1
        DO i = cv_start(1),cv_count(1)
          rtmp(i) = rF(i + 1)
        ENDDO
C
        IF ( usingZCoords ) THEN
         long_name = 'vertical coordinate of lower cell interface'
         units     = 'meters'
         positive  = 'up'
        ELSEIF ( usingPCoords ) THEN
         long_name = 'vertical coordinate of upper cell interface'
         units     = 'pascal'
        ELSE
C       unknown grid type
         print *, 'S/R MNC_CW_CVARS: Ooops, unknown vertical grid!'
        ENDIF

      ELSE

        doit = 0

      ENDIF

      IF ( doit .EQ. 1 ) THEN

        CALL MNC_FILE_REDEF(fname, myThid)
#ifdef REAL4_IS_SLOW
        err = NF_DEF_VAR(fid, cvname, NF_DOUBLE,
     &       nids, cv_did, vid)
#else
        err = NF_DEF_VAR(fid, cvname, NF_FLOAT,
     &       nids, cv_did, vid)
#endif /* REAL4_IS_SLOW */
        i = ILNBLNK( fname )
        write(msgbuf,'(5a)') 'defining coordinate variable ''',
     &       cvname(nnf:nnl), ''' in file ''', fname(1:i), ''''
        CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
C     add attributes if set
        ia = ILNBLNK(long_name)
        IF ( ia .GT. 0 ) THEN
         err = NF_PUT_ATT_TEXT(fid, vid, 'long_name', ia, long_name)
         write(msgbuf,'(5a)')
     &     'adding attribute ''long_name'' to coordinate variable ''',
     &     cvname(nnf:nnl), ''' in file ''', fname(1:i), ''''
         CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
        ENDIF
        ia = ILNBLNK(units)
        IF ( ia .GT. 0 ) THEN
         err = NF_PUT_ATT_TEXT(fid, vid, 'units', ia, units)
         write(msgbuf,'(5a)')
     &     'adding attribute ''units'' to coordinate variable ''',
     &     cvname(nnf:nnl), ''' in file ''', fname(1:i), ''''
         CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
        ENDIF
        ia = ILNBLNK(positive)
        IF ( ia .GT. 0 ) THEN
         err = NF_PUT_ATT_TEXT(fid, vid, 'positive', ia, positive)
         write(msgbuf,'(5a)')
     &     'adding attribute ''positive'' to coordinate variable ''',
     &     cvname(nnf:nnl), ''' in file ''', fname(1:i), ''''
         CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
        ENDIF
C
        CALL MNC_FILE_ENDDEF(fname, myThid)
#ifdef REAL4_IS_SLOW
        err = NF_PUT_VARA_DOUBLE(fid, vid,
     &       cv_start, cv_count, rtmp)
#else
        err = NF_PUT_VARA_REAL(fid, vid,
     &       cv_start, cv_count, rtmp)
#endif /* REAL4_IS_SLOW */
        write(msgbuf,'(5a)') 'writing coordinate variable ''',
     &       cvname(nnf:nnl), ''' in file ''', fname(1:i), ''''
        CALL MNC_HANDLE_ERR(err, msgbuf, myThid)

      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

