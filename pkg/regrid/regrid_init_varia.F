C $Header: /u/gcmpack/MITgcm/pkg/regrid/regrid_init_varia.F,v 1.3 2009/06/28 01:05:41 jmc Exp $
C $Name:  $

#include "REGRID_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: REGRID_INIT_VARIA

C     !INTERFACE:
      SUBROUTINE REGRID_INIT_VARIA( myThid )

C     !DESCRIPTION:
C     Initialize REGRID variables

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "REGRID_SIZE.h"
#include "REGRID.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i,k, iface, uniq_tnum, bi,bj
      INTEGER irx, isrc, idst, nFx,nFy, init_nlpts,nlpts
      INTEGER iUnit, errIO, nnb
      INTEGER iminx,iminy, imaxx,imaxy
      _RL wt
      CHARACTER*(MAX_LEN_FNAM) fname
      CHARACTER*(MAX_LEN_MBUF) msgbuf
      LOGICAL  exst

C     Regrid files contain information on a per-face basis.  This is
C     convenient in two respects: (1) the domain can be re-tiled without
C     changing any of the files [since the ordering with respect to
C     tiles is performed here in the model] and (2) when faces are
C     removed or added only the corresponding per-face files will need
C     to be removed or added [and all the other per-face files remain
C     unchanged provided the face numbers do not change].
C
C     The convention is: "points cycle most quickly in X and then Y"
C
C        +-------------------+
C        |  Face             |
C        |                   |
C        |       +-----+     |
C      Y |       |Tile |     |
C        |       +-----+     |
C        |                   |
C        |123...             |
C        +-------------------+
C                X

      _BEGIN_MASTER( myThid )

      WRITE(msgBuf,'(a)')
     &     '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &     SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(a)')
     &     '// Begin reading the per-face REGRID information'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &     SQUEEZE_RIGHT,myThid)
      WRITE(msgBuf,'(a)')
     &     '// ======================================================='
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &     SQUEEZE_RIGHT,myThid)

      nlpts = 0

      CALL MDSFINDUNIT(iUnit, myThid)

      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)

#ifdef ALLOW_EXCH2
C         EXCH2 domains
          uniq_tnum = W2_myTileList(bi,bj)
          iface = exch2_myFace(uniq_tnum)
          nFx = exch2_mydnx(uniq_tnum)
          nFy = exch2_mydny(uniq_tnum)
          iminx = exch2_tbasex(uniq_tnum) + 1
          imaxx = iminx + exch2_tnx(uniq_tnum) - 1
          iminy = exch2_tbasey(uniq_tnum) + 1
          imaxy = iminy + exch2_tny(uniq_tnum) - 1
#else
C         Global tile number for simple single-face "EXCH1" domains
          iG = bi + (myXGlobalLo-1)/sNx
          jG = bj + (myYGlobalLo-1)/sNy
          uniq_tnum = (jG - 1)*(nPx*nSx) + iG
          iface = 1
          nFx = nSx * sNx
          nFy = sSy * sNy
          iminx = myXGlobalLo
          imaxx = myXGlobalLo + sNx - 1
          iminy = myYGlobalLo
          imaxy = myYGlobalLo + sNy - 1
#endif

C         WRITE(*,*) 'iminx, imaxx, nFx, nFy = ',
C         &         iminx, imaxx, nFx, nFy

C         Read through all the weights files for this tile (face) and
C         locate the points that belong to this tile
          DO i = 1,regrid_ngrids

            IF (i .EQ. 1) THEN
              nlpts = 0
            ELSE
              nlpts = REGRID_iend(i,bi,bj)
            ENDIF
            init_nlpts = nlpts

            DO k = 1,MAX_LEN_FNAM
              fname(k:k) = ' '
            ENDDO
            nnb = ILNBLNK(REGRID_fbname_in(i))
            write(fname,'(a,i3.3,a)')
     &           REGRID_fbname_in(i)(1:nnb),iface,'.regrid.ascii'
            nnb = ILNBLNK(fname)
            INQUIRE( FILE=fname, EXIST=exst )
            IF (.NOT. exst) THEN
              WRITE(msgBuf,'(A)')  'S/R REGRID_INIT_VARIA()'
              CALL PRINT_ERROR( msgBuf , 1)
              WRITE(msgBuf,'(3A)')  ' File "',
     &             fname(1:nnb), '" does not exist'
              CALL PRINT_ERROR( msgBuf , 1)
              CLOSE(iUnit)
              STOP ' stopped in REGRID_INIT_VARIA()'
            ENDIF

            open(unit=iUnit, file=fname, status='old', iostat=errIO)

            IF (errIO .LT. 0) THEN
              WRITE(msgBuf,'(A)')  'S/R REGRID_INIT_VARIA()'
              CALL PRINT_ERROR( msgBuf , 1)
              WRITE(msgBuf,'(3A)')  'Unable to open file="',
     &             fname(1:nnb), '"'
              CALL PRINT_ERROR( msgBuf , 1)
              CLOSE(iUnit)
              STOP ' stopped in REGRID_INIT_VARIA()'
            ELSE
              WRITE(msgBuf,'(3a)') 'Reading file "', fname(1:nnb),'"'
              call PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &             SQUEEZE_RIGHT,myThid)
            ENDIF

            DO WHILE ( .TRUE. )
C             READ(iUnit,fmt='(2(I10,1X),1P1E23.13E3)',iostat=errIO)
              READ(iUnit,fmt='(2(1X,I10),1X,E28.22)',iostat=errIO)
     &             isrc, idst, wt
              IF ( errIO .NE. 0 ) THEN
                GOTO 100
              ENDIF
              irx = MOD(isrc,nFx)
              IF (irx .EQ. 0)  irx = nFx
              IF ((iminx .LE. irx) .AND. (irx .LE. imaxx)) THEN
                nlpts = nlpts + 1
                REGRID_i_loc(nlpts,bi,bj) = irx
                REGRID_j_loc(nlpts,bi,bj) = isrc/nFx + 1
                REGRID_i_out(nlpts,bi,bj) = idst
                REGRID_amat(nlpts,bi,bj)  = wt
              ENDIF

            ENDDO
 100        CONTINUE
            close(iUnit)
            WRITE(msgBuf,'(a,i10)') '  num weights read = ',
     &           (nlpts - init_nlpts)
            call PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &           SQUEEZE_RIGHT,myThid)

            REGRID_ibeg(i,bi,bj) = init_nlpts + 1
            REGRID_iend(i,bi,bj) = nlpts
          ENDDO

        ENDDO
      ENDDO

      WRITE(msgBuf,'(a)') ' '
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &     SQUEEZE_RIGHT,myThid)

      _END_MASTER( myThid )

      RETURN
      END
