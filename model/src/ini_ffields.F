#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_FFIELDS
C     !INTERFACE:
      SUBROUTINE INI_FFIELDS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_FFIELDS
C     | o Initialise to zero FFLIEDS.h arrays
C     *==========================================================*
C     | Sets all forcing fields to zero.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid  :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj   :: Tile indices
C     i, j, k :: Loop counters
      INTEGER bi, bj
      INTEGER i, j, k
CEOP

C--   Initialise all arrays in common blocks
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-    2-D forcing fields
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fu              (i,j,bi,bj) = 0. _d 0
          fv              (i,j,bi,bj) = 0. _d 0
          Qnet            (i,j,bi,bj) = 0. _d 0
          Qsw             (i,j,bi,bj) = 0. _d 0
          EmPmR           (i,j,bi,bj) = 0. _d 0
          saltFlux        (i,j,bi,bj) = 0. _d 0
          SST             (i,j,bi,bj) = 0. _d 0
          SSS             (i,j,bi,bj) = 0. _d 0
          phiTide2d       (i,j,bi,bj) = 0. _d 0
          pLoad           (i,j,bi,bj) = 0. _d 0
          sIceLoad        (i,j,bi,bj) = 0. _d 0
          gcmSST          (i,j,bi,bj) = 0. _d 0
#ifdef ALLOW_GEOTHERMAL_FLUX
          geothermalFlux  (i,j,bi,bj) = 0. _d 0
#endif
#ifndef EXCLUDE_FFIELDS_LOAD
C-    Time reccord loaded from file:
          taux0           (i,j,bi,bj) = 0. _d 0
          taux1           (i,j,bi,bj) = 0. _d 0
          tauy0           (i,j,bi,bj) = 0. _d 0
          tauy1           (i,j,bi,bj) = 0. _d 0
          Qnet0           (i,j,bi,bj) = 0. _d 0
          Qnet1           (i,j,bi,bj) = 0. _d 0
          EmPmR0          (i,j,bi,bj) = 0. _d 0
          EmPmR1          (i,j,bi,bj) = 0. _d 0
          saltFlux0       (i,j,bi,bj) = 0. _d 0
          saltFlux1       (i,j,bi,bj) = 0. _d 0
          SST0            (i,j,bi,bj) = 0. _d 0
          SST1            (i,j,bi,bj) = 0. _d 0
          SSS0            (i,j,bi,bj) = 0. _d 0
          SSS1            (i,j,bi,bj) = 0. _d 0
#ifdef SHORTWAVE_HEATING
          Qsw0            (i,j,bi,bj) = 0. _d 0
          Qsw1            (i,j,bi,bj) = 0. _d 0
#endif
#ifdef ALLOW_GEOTHERMAL_FLUX
          geothFlux0      (i,j,bi,bj) = 0. _d 0
          geothFlux1      (i,j,bi,bj) = 0. _d 0
#endif
#ifdef ATMOSPHERIC_LOADING
          pLoad0          (i,j,bi,bj) = 0. _d 0
          pLoad1          (i,j,bi,bj) = 0. _d 0
#endif
#endif /* EXCLUDE_FFIELDS_LOAD */
         ENDDO
        ENDDO
#ifndef EXCLUDE_FFIELDS_LOAD
        loadedRec(bi,bj) = 0
#endif /* EXCLUDE_FFIELDS_LOAD */

C-    3-D forcing fields
        DO k=1,Nr
#ifdef ALLOW_ADDFLUID
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           addMass(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
#endif /* ALLOW_ADDFLUID */
#ifdef ALLOW_FRICTION_HEATING
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           frictionHeating(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
#endif /* ALLOW_FRICTION_HEATING */
#ifdef ALLOW_EDDYPSI
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           eddyPsiX  (i,j,k,bi,bj) = 0. _d 0
           eddyPsiY  (i,j,k,bi,bj) = 0. _d 0
           tauxEddy  (i,j,k,bi,bj) = 0. _d 0
           tauyEddy  (i,j,k,bi,bj) = 0. _d 0
           uEulerMean(i,j,k,bi,bj) = 0. _d 0
           vEulerMean(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
#endif /* ALLOW_EDDYPSI */
        ENDDO

C-    Intermediate surface forcing arrays
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          surfaceForcingU (i,j,bi,bj) = 0. _d 0
          surfaceForcingV (i,j,bi,bj) = 0. _d 0
          surfaceForcingT (i,j,bi,bj) = 0. _d 0
          surfaceForcingS (i,j,bi,bj) = 0. _d 0
          adjustColdSST_diag  (i,j,bi,bj) = 0. _d 0
          lambdaThetaClimRelax(i,j,bi,bj) = 0. _d 0
          lambdaSaltClimRelax (i,j,bi,bj) = 0. _d 0
#ifdef ALLOW_BALANCE_FLUXES
          weight2BalanceFlx(i,j,bi,bj) = 0. _d 0
#endif
          botDragU        (i,j,bi,bj) = 0. _d 0
          botDragV        (i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO

C--   end bi,bj loops
       ENDDO
      ENDDO

      RETURN
      END
