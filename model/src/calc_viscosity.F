C $Header: /u/gcmpack/MITgcm/model/src/calc_viscosity.F,v 1.3 2001/09/26 18:09:14 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_VISCOSITY
C     !INTERFACE:
      SUBROUTINE CALC_VISCOSITY( 
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     O        KappaRU,KappaRV,
     I        myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_VISCOSITY                                 
C     | o Calculate net vertical viscosity                        
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C     myThid - Instance number for this innvocation of CALC_COMMON_FACTORS
C     KappaRU :: Total vertical viscosity for zonal flow.
C     KappaRV :: Total vertical viscosity for meridional flow.
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL KappaRU(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRV(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     I, J, K - Loop counters
      INTEGER i,j
CEOP

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRU(i,j,k) = viscAr
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRV(i,j,k) = viscAr
       ENDDO
      ENDDO

#ifdef ALLOW_KPP
      IF (useKPP) CALL KPP_CALC_VISC(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,K,
     O        KappaRU,KappaRV,
     I        myThid)
#endif


      RETURN
      END
