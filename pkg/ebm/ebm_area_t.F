#include "EBM_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP 0
C !ROUTINE: EBM_AREA_T

C !INTERFACE:
      SUBROUTINE EBM_AREA_T( myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | CALCULATE MEAN TEMPERATURE OVER 4 LAT. BAND
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_EBM
# include "EBM.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT PARAMETERS:
C     myThid     :: my Thread Id number
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_EBM
C     !LOCAL VARIABLES:
C     i, j, k  :: Loop counters
      INTEGER i, j, k, bi, bj
      _RL TmlS_tile(nSx, nSy)
      _RL TmlN_tile(nSx, nSy)
      _RL TtS_tile(nSx, nSy)
      _RL TtN_tile(nSx, nSy)
      _RL CountmlS_tile(nSx, nSy)
      _RL CountmlN_tile(nSx, nSy)
      _RL CounttS_tile(nSx, nSy)
      _RL CounttN_tile(nSx, nSy)
      _RL CountmlS
      _RL CountmlN
      _RL CounttS
      _RL CounttN

C--   Top layer only
      k = 1

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        TmlS_tile(bi,bj) = 0.
        TmlN_tile(bi,bj) = 0.
        TtS_tile(bi,bj) = 0.
        TtN_tile(bi,bj) = 0.
        CountmlS_tile(bi,bj) = 0.
        CountmlN_tile(bi,bj) = 0.
        CounttS_tile(bi,bj) = 0.
        CounttN_tile(bi,bj) = 0.

C--   Calculate sum of temperatures over each tile
        DO j = 1,sNy
         DO i = 1, sNx
          IF ( yC(i,j,bi,bj) .GE. -latBnd(3) .AND.
     &         yC(i,j,bi,bj) .LE. -latBnd(2) ) THEN
            TmlS_tile(bi,bj) = TmlS_tile(bi,bj)
     &                       + maskC(i,j,k,bi,bj)*theta(i,j,k,bi,bj)
            CountmlS_tile(bi,bj) = CountmlS_tile(bi,bj)
     &                       + maskC(i,j,k,bi,bj)
          ELSEIF ( yC(i,j,bi,bj) .GT. -latBnd(2) .AND.
     &             yC(i,j,bi,bj) .LT. -latBnd(1) ) THEN
            TtS_tile(bi,bj) = TtS_tile(bi,bj)
     &                       + maskC(i,j,k,bi,bj)*theta(i,j,k,bi,bj)
            CounttS_tile(bi,bj) = CounttS_tile(bi,bj)
     &                       + maskC(i,j,k,bi,bj)
          ELSEIF ( yC(i,j,bi,bj) .GT. latBnd(1) .AND.
     &             yC(i,j,bi,bj) .LT. latBnd(2) ) THEN
            TtN_tile(bi,bj) = TtN_tile(bi,bj)
     &                       + maskC(i,j,k,bi,bj)*theta(i,j,k,bi,bj)
            CounttN_tile(bi,bj) = CounttN_tile(bi,bj)
     &                       + maskC(i,j,k,bi,bj)
          ELSEIF ( yC(i,j,bi,bj) .GE. latBnd(2) .AND.
     &             yC(i,j,bi,bj) .LT. latBnd(3) ) THEN
            TmlN_tile(bi,bj) = TmlN_tile(bi,bj)
     &                       + maskC(i,j,k,bi,bj)*theta(i,j,k,bi,bj)
            CountmlN_tile(bi,bj) = CountmlN_tile(bi,bj)
     &                       + maskC(i,j,k,bi,bj)
          ENDIF
         ENDDO
        ENDDO

       ENDDO
      ENDDO

      CALL GLOBAL_SUM_TILE_RL( TmlS_tile, TmlS, myThid )
      CALL GLOBAL_SUM_TILE_RL( TmlN_tile, TmlN, myThid )
      CALL GLOBAL_SUM_TILE_RL( TtS_tile,  TtS,  myThid )
      CALL GLOBAL_SUM_TILE_RL( TtN_tile,  TtN,  myThid )

      CALL GLOBAL_SUM_TILE_RL( CountmlS_tile, CountmlS, myThid )
      CALL GLOBAL_SUM_TILE_RL( CountmlN_tile, CountmlN, myThid )
      CALL GLOBAL_SUM_TILE_RL( CounttS_tile,  CounttS,  myThid )
      CALL GLOBAL_SUM_TILE_RL( CounttN_tile,  CounttN,  myThid )

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE CountmlS = comlev1, key = ikey_dynamics
CADJ STORE CountmlN = comlev1, key = ikey_dynamics
CADJ STORE CounttS  = comlev1, key = ikey_dynamics
CADJ STORE CounttN  = comlev1, key = ikey_dynamics
#endif
      _BEGIN_MASTER(myThid)
      IF ( CountmlS .NE. 0.) TmlS = TmlS/CountmlS
      IF ( CountmlN .NE. 0.) TmlN = TmlN/CountmlN
      IF ( CounttS  .NE. 0.) TtS  = TtS /CounttS
      IF ( CounttN  .NE. 0.) TtN  = TtN /CounttN
      TmlS = TmlS + t_mlt
      TmlN = TmlN + t_mlt
      TtS  = TtS  + t_mlt
      TtN  = TtN  + t_mlt
      _END_MASTER(myThid)
      _BARRIER

#endif /* ALLOW_EBM */

      RETURN
      END
