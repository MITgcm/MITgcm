C $Header: /u/gcmpack/MITgcm/model/src/calc_ivdc.F,v 1.1 1999/08/26 17:47:37 adcroft Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE CALC_IVDC( bi, bj, iMin, iMax, jMin, jMax, K,
     &       rhoKm1, rhoKp1,
     &       ConvectCount, KappaRT, KappaRS,
     &       myTime,myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE CALC_IVDC                                     |
C     | o Calculates Implicit Vertical Diffusivity for Convection|
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     bi,bj,iMin,iMax,jMin,jMax,K - Loop counters
C     rhoKm1 - rho in layer above
C     rhoKp1 - rho in layer below
C     myTime - Current time in simulation
C     myIter - Current iteration in simulation
C     myThid - Thread number of this instance of S/R CONVECT
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL rhoKm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoKp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ConvectCount(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

C--    Where statically unstable, mix the heat and salt
       DO j=jMin,jmax
        DO i=iMin,imax
         IF ( _hFacC(i,j,k,bi,bj) .GT. 0. .AND.
     &        rhokm1(i,j) .GT. rhokp1(i,j) ) THEN
          KappaRT(i,j,k) = ivdc_kappa
          KappaRS(i,j,k) = ivdc_kappa
          ConvectCount(i,j,k) = ConvectCount(i,j,k) + 1.
         ENDIF
        ENDDO
       ENDDO
       
      RETURN
      END
