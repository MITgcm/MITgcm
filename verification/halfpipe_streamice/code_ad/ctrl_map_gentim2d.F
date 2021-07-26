#include "CTRL_OPTIONS.h"
#ifdef ALLOW_STREAMICE
# include "STREAMICE_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: CTRL_MAP_GENTIM2D
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_GENTIM2D(
     I                        myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  CTRL_MAP_GENTIM2D
C     *=============================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "CTRL_GENARR.h"
#include "ctrl_dummy.h"
#ifdef ALLOW_AUTODIFF
#include "AUTODIFF_MYFIELDS.h"
#endif
#ifdef ALLOW_STREAMICE
# include "STREAMICE.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL  myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GENTIM2D_CONTROL
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi, bj
      INTEGER i, j
      INTEGER iarr
      _RL xx_gentim2d_loc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS mask2D         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL LOCsumTile(nSx,nSy), LOCsumGlob
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   generic - user-defined control vars
      DO iarr = 1, maxCtrlTim2D

       IF (xx_gentim2d_weight(iarr).NE.' ') THEN

        DO bj = myByLo(myThid),myByHi(myThid)
         DO bi = myBxLo(myThid),myBxHi(myThid)
          DO j = 1-OLy,sNy+OLy
           DO i = 1-OLx,sNx+OLx
            xx_gentim2d_loc(i,j,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO

        CALL CTRL_GET_MASK2D( xx_gentim2d_file(iarr), mask2D, myThid )
        CALL CTRL_GET_GEN (
     I      xx_gentim2d_file(iarr),
     I      xx_gentim2d_startdate(1,iarr),
     I      xx_gentim2d_period(iarr),
     I      mask2D,
     O      xx_gentim2d_loc,
     I      xx_gentim2d0(1-OLx,1-OLy,1,1,iarr),
     I      xx_gentim2d1(1-OLx,1-OLy,1,1,iarr),
     I      xx_gentim2d_dummy(iarr),
     I      zeroRL, zeroRL,
     I      wgentim2d(1-OLx,1-OLy,1,1,iarr),
     I      myTime, myIter, myThid )

        IF (xx_gentim2d_cumsum(iarr)) THEN
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
             xx_gentim2d(i,j,bi,bj,iarr)=xx_gentim2d(i,j,bi,bj,iarr)
     &            + xx_gentim2d_loc(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ELSE
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
             xx_gentim2d(i,j,bi,bj,iarr)=xx_gentim2d_loc(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF

        IF (xx_gentim2d_glosum(iarr)) THEN

         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           LOCsumTile(bi,bj)=0. _d 0
           DO j = 1,sNy
            DO i = 1,sNx
             LOCsumTile(bi,bj) = LOCsumTile(bi,bj)
     &          + xx_gentim2d(i,j,bi,bj,iarr)*rA(i,j,bi,bj)
     &           *maskC(i,j,1,bi,bj)*maskInC(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO

         CALL GLOBAL_SUM_TILE_RL( LOCsumTile, LOCsumGlob, myThid )

         LOCsumGlob = LOCsumGlob/globalArea
         DO bj = myByLo(myThid),myByHi(myThid)
          DO bi = myBxLo(myThid),myBxHi(myThid)
           DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
             xx_gentim2d(i,j,bi,bj,iarr) =
     &                   LOCsumGlob*maskC(i,j,1,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO

         IF (xx_gentim2d_file(iarr).EQ.'xx_gen_precip') THEN

          WRITE(msgBuf,'(A,I6,A,1PE21.14)')
     &         ' iter=', myIter, ' ; genprecipGloH= ',
     &         LOCsumGlob*rhoConstFresh*recip_rhoConst*deltaTClock
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )

         ENDIF !IF (xx_gentim2d_file(iarr).EQ.'xx_gen_precip') THEN

        ENDIF !IF (xx_gentim2d_glosum(iarr)) THEN

C---  begin customized code for experiment halfpipe_streamice:
        IF ( iarr .EQ. 1 ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
             bdot_streamice(i,j,bi,bj) = xx_gentim2d(i,j,bi,bj,iarr)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
C---  end customized code

       ENDIF !IF (xx_gentim2d_weight(iarr).NE.' ') THEN

      ENDDO !DO iarr = 1, maxCtrlTim2D

#endif /* ALLOW_GENTIM2D_CONTROL */

      RETURN
      END
