
#include "CPP_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE CALC_VISCOSITY( 
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     O        KappaRU,KappaRV,
     I        myThid)

C     /==========================================================\
C     | SUBROUTINE CALC_VISCOSITY                                |
C     | o Calculate net viscosity                                |
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

C     == Routine arguments ==
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C     myThid - Instance number for this innvocation of CALC_COMMON_FACTORS
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL KappaRU(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRV(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I, J, K - Loop counters
      INTEGER i,j

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
