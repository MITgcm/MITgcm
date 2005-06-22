C $Header: /u/gcmpack/MITgcm/model/src/calc_ivdc.F,v 1.7 2005/06/22 00:25:32 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
CBOP
C     !ROUTINE: CALC_IVDC
C     !INTERFACE:
      SUBROUTINE CALC_IVDC( 
     I       bi, bj, iMin, iMax, jMin, jMax, K,
     I       rhoKm1, rhoKp1,
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
C     myThid :: Thread number of this instance of S/R CONVECT
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL rhoKm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoKp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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
     &        (rhokm1(i,j)-rhokp1(i,j))*rkSign*gravitySign.GT.0. ) THEN
          IVDConvCount(i,j,k,bi,bj) = 1. _d 0
         ELSE
          IVDConvCount(i,j,k,bi,bj) = 0. _d 0
         ENDIF
        ENDDO
       ENDDO
       
      RETURN
      END
