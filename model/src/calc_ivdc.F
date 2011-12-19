C $Header: /u/gcmpack/MITgcm/model/src/calc_ivdc.F,v 1.8 2011/12/19 11:47:04 mlosch Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
CBOP
C     !ROUTINE: CALC_IVDC
C     !INTERFACE:
      SUBROUTINE CALC_IVDC( 
     I       bi, bj, iMin, iMax, jMin, jMax, K,
     I       sigmaR,
     I       myTime,myIter, myThid )
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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj,iMin,iMax,jMin,jMax,K :: Loop counters
C     rhoKm1 :: rho in layer above
C     rhoKp1 :: rho in layer below
C     myTime :: Current time in simulation
C     myIter :: Current iteration in simulation
C     myThid :: Thread number of this instance of S/R CALC_IVDC
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL sigmaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
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
         IF ( -sigmaR(i,j,k)*gravitySign.GT.0. ) THEN
          IVDConvCount(i,j,k,bi,bj) = 1. _d 0
         ELSE
          IVDConvCount(i,j,k,bi,bj) = 0. _d 0
         ENDIF
        ENDDO
       ENDDO
       
      RETURN
      END
