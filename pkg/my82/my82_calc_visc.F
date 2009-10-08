C $Header: /u/gcmpack/MITgcm/pkg/my82/my82_calc_visc.F,v 1.4 2009/10/08 20:07:53 jmc Exp $
C $Name:  $
#include "MY82_OPTIONS.h"

      SUBROUTINE MY82_CALC_VISC(
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     U        KappaRU,KappaRV,
     I        myThid)

CBOP
C     *==========================================================*
C     | SUBROUTINE MY82_CALC_VISC
C     | o Add contrubution to net viscosity from MY82 mixing
C     *==========================================================*
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "MY82.h"

C     == Routine arguments ==
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C     myThid - Instance number for this innvocation of CALC_COMMON_FACTORS
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL KappaRU(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRV(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid

#ifdef ALLOW_MY82

C     == Local variables ==
C     I, J, K - Loop counters
      INTEGER i,j
CEOP
      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRU(i,j,k) = max ( KappaRU(i,j,k),
     &         KappaRU(i,j,k) - viscArNr(k) + _maskW(i,j,k,bi,bj) *
     &    0.5 _d 0*(MYviscAr(i,j,k,bi,bj)+MYviscAr(i-1,j,k,bi,bj)) )
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRV(i,j,k) = max ( KappaRV(i,j,k),
     &         KappaRV(i,j,k) - viscArNr(k) + _maskS(i,j,k,bi,bj) *
     &    0.5 _d 0*(MYviscAr(i,j,k,bi,bj)+MYviscAr(i,j-1,k,bi,bj)) )
       ENDDO
      ENDDO

#endif /* ALLOW_MY82 */

      RETURN
      END
