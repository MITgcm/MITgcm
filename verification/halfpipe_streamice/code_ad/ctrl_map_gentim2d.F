#include "CTRL_OPTIONS.h"
#include "STREAMICE_OPTIONS.h"

CBOP
C     !ROUTINE: CTRL_MAP_GENTIM2D
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_GENTIM2D(
     I                        myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  CTRL_MAP_GENTIM2D
C     *=============================================================*

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
#include "STREAMICE.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL  myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GENTIM2D_CONTROL
C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj
      INTEGER i,j
      INTEGER iarr

      _RL xx_gentim2d_loc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS mask2D         (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL LOCsumTile(nSx,nSy), LOCsumGlob

CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   generic - user-defined control vars

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO J = 1-Oly,sNy+Oly
          DO I = 1-Olx,sNx+Olx
           bdot_streamice(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      DO iarr = 1, maxCtrlTim2D

       IF (xx_gentim2d_weight(iarr).NE.' ') THEN

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j = 1-OLy,sNy+OLy
           DO i = 1-OLx,sNx+OLx
            xx_gentim2d_loc(i,j,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
C
!       CALL CTRL_GET_GEN (
!     I      xx_gentim2d_file(iarr)(1:MAX_LEN_FNAM),
!     I      xx_gentim2d_startdate(1,iarr),
!     I      xx_gentim2d_period(iarr),
!     I      maskC,
!     O      xx_gentim2d_loc,
!     I      xx_gentim2d0(1-Olx,1-Oly,1,1,iarr),
!     I      xx_gentim2d1(1-Olx,1-Oly,1,1,iarr),
!     I      xx_gentim2d_dummy(iarr),
!     I      zeroRL, zeroRL,
!     I      mytime, myiter, mythid )

!       CALL CTRL_GET_GEN (
!     I      xx_gentim2d_file(iarr)(1:MAX_LEN_FNAM),
!     I      xx_gentim2d_startdate(1,iarr),
!     I      xx_gentim2d_period(iarr),
!     I      maskC,
!     O      xx_gentim2d_loc,
!     I      xx_gentim2d0(1-Olx,1-Oly,1,1,iarr),
!     I      xx_gentim2d1(1-Olx,1-Oly,1,1,iarr),
!     I      xx_gentim2d_dummy(iarr),
!     I      zeroRL, zeroRL, genweight,
!     I      mytime, myiter, mythid )

        CALL CTRL_GET_MASK2D( xx_gentim2d_file(iarr)(1:MAX_LEN_FNAM),
     &      mask2D, myThid )
        CALL CTRL_GET_GEN (
     I      xx_gentim2d_file(iarr)(1:MAX_LEN_FNAM),
     I      xx_gentim2d_startdate(1,iarr),
     I      xx_gentim2d_period(iarr),
     I      mask2D,
     O      xx_gentim2d_loc,
     I      xx_gentim2d0(1-OLx,1-OLy,1,1,iarr),
     I      xx_gentim2d1(1-OLx,1-OLy,1,1,iarr),
     I      xx_gentim2d_dummy(iarr),
     I      zeroRL, zeroRL,
     I      wgentim2d(1-OLx,1-OLy,1,1,iarr),
     I      mytime, myiter, mythid )
C
        IF (xx_gentim2d_cumsum(iarr)) THEN
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
             xx_gentim2d(i,j,bi,bj,iarr)=xx_gentim2d(i,j,bi,bj,iarr)
     &            +xx_gentim2d_loc(i,j,bi,bj)
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
C
        IF (xx_gentim2d_glosum(iarr)) THEN

         LOCsumGlob=0. _d 0
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           LOCsumTile(bi,bj)=0. _d 0
           DO j = 1,sNy
            DO i = 1,sNx
             LOCsumTile(bi,bj)=LOCsumTile(bi,bj)+
     &            maskC(i,j,1,bi,bj)*rA(i,j,bi,bj)
     &            *xx_gentim2d(i,j,bi,bj,iarr)
            ENDDO
           ENDDO
          ENDDO
         ENDDO

         CALL GLOBAL_SUM_TILE_RL( LOCsumTile, LOCsumGlob, myThid )

         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
             xx_gentim2d(i,j,bi,bj,iarr) =
     &            LOCsumGlob/globalArea*maskC(i,j,1,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO

        ENDIF !IF (xx_gentim2d_glosum(iarr)) THEN

C--   begin customized code for experiment hs94.1x64x5
        IF ( iarr .EQ. 1 ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j = 1-OLy,sNy+OLy
            DO i = 1-Olx,sNx+OLx
             bdot_streamice(i,j,bi,bj) =
     &            bdot_streamice(i,j,bi,bj)+
     &            xx_gentim2d(i,j,bi,bj,iarr)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
C--   end customized code

       ENDIF !IF (xx_gentim2d_weight(iarr).NE.' ') THEN

      ENDDO !DO iarr = 1, maxCtrlTim2D


#endif /* ALLOW_GENTIM2D_CONTROL */

      RETURN
      END
