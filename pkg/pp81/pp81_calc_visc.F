C $Header: /u/gcmpack/MITgcm/pkg/pp81/pp81_calc_visc.F,v 1.3 2009/10/08 20:08:20 jmc Exp $
C $Name:  $
#include "PP81_OPTIONS.h"

      SUBROUTINE PP81_CALC_VISC(
     I        bi,bj,iMin,iMax,jMin,jMax,k,
     U        KappaRU,KappaRV,
     I        myThid)

CBOP
C     *==========================================================*
C     | SUBROUTINE PP81_CALC_VISC                                |
C     | o Add contrubution to net viscosity from PP81 mixing     |
C     *==========================================================*
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "PP81.h"

C     == Routine arguments ==
C     bi, bj :: tile numbers
C     iMin, iMax, jMin, jMax :: Range of points for which calculation
C     maskUp :: land/water mask for Wvel points (above tracer level)
C     myThid :: Instance number for this innvocation of PP81_CALC_VISC
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,k
      _RL KappaRU(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRV(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid

#ifdef ALLOW_PP81

C     == Local variables ==
C     i, j    :: Loop counters
      INTEGER i,j
CEOP
      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRU(i,j,k) = max ( KappaRU(i,j,k),
     &         KappaRU(i,j,k) - viscArNr(k) + _maskW(i,j,k,bi,bj) *
     &         0.5*(PPviscAr(i,j,k,bi,bj)+PPviscAr(i-1,j,k,bi,bj)) )
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRV(i,j,k) = max ( KappaRV(i,j,k),
     &         KappaRV(i,j,k) - viscArNr(k) + _maskS(i,j,k,bi,bj) *
     &         0.5*(PPviscAr(i,j,k,bi,bj)+PPviscAr(i,j-1,k,bi,bj)) )
       ENDDO
      ENDDO

#endif /* ALLOW_PP81 */

      RETURN
      END
