C $Header: /u/gcmpack/MITgcm/model/src/calc_ivdc.F,v 1.3 2001/09/26 18:09:14 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
CBOP
C     !ROUTINE: CALC_IVDC
C     !INTERFACE:
      SUBROUTINE CALC_IVDC( bi, bj, iMin, iMax, jMin, jMax, K,
     &       rhoKm1, rhoKp1,
     &       ConvectCount, KappaRT, KappaRS,
     &       myTime,myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_IVDC                                      
C     | o Calculates Implicit Vertical Diffusivity for Convection 
C     \==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
      EXTERNAL DIFFERENT_MULTIPLE
      LOGICAL  DIFFERENT_MULTIPLE

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj,iMin,iMax,jMin,jMax,K :: Loop counters
C     rhoKm1 :: rho in layer above
C     rhoKp1 :: rho in layer below
C     myTime :: Current time in simulation
C     myIter :: Current iteration in simulation
C     myThid :: Thread number of this instance of S/R CONVECT
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
C     KappaRT :: Vertical diffusion coeff. for theta
C     KappaRS :: Vertical diffusion coeff. for salt 
C     ConvectCount :: Static instability counter
      _RL rhoKm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoKp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ConvectCount(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j :: Loop counters
      INTEGER i,j
CEOP

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
