C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_calc_visc.F,v 1.2 2007/07/31 23:01:58 ce107 Exp $
C $Name:  $
#include "GGL90_OPTIONS.h"

      SUBROUTINE GGL90_CALC_VISC( 
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     U        KappaRU,KappaRV,
     I        myThid)

CBOP
C     /==========================================================\
C     | SUBROUTINE GGL90_CALC_VISC                               |
C     | o Add contrubution to net viscosity from GGL90 mixing    |
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
C     myThid - Instance number for this innvocation of GGL90_CALC_VISC
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL KappaRU(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRV(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid

#ifdef ALLOW_GGL90
#include "GGL90.h"

C     == Local variables ==
C     I, J, K - Loop counters
      INTEGER i,j
CEOP
      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRU(i,j,k) = max ( KappaRU(i,j,k),
     &     KappaRU(i,j,k) - viscAr + _maskW(i,j,k,bi,bj) *
     &     0.5*(GGL90viscAr(i,j,k,bi,bj)+GGL90viscAr(i-1,j,k,bi,bj)) )
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRV(i,j,k) = max ( KappaRV(i,j,k),
     &     KappaRV(i,j,k) - viscAr + _maskS(i,j,k,bi,bj) *
     &     0.5*(GGL90viscAr(i,j,k,bi,bj)+GGL90viscAr(i,j-1,k,bi,bj)) )
       ENDDO
      ENDDO

#endif /* ALLOW_GGL90 */

      RETURN
      END
