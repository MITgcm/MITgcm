C $Header: /u/gcmpack/MITgcm/pkg/kl10/kl10_calc_visc.F,v 1.1 2014/07/30 03:28:05 jmc Exp $
C $Name:  $
#include "KL10_OPTIONS.h"

      SUBROUTINE KL10_CALC_VISC(
     I        bi,bj,iMin,iMax,jMin,jMax,k,
     U        KappaRU,KappaRV,
     I        myThid)

CBOP
C     *==========================================================*
C     | SUBROUTINE KL10_CALC_VISC                                |
C     | o Add contrubution to net viscosity from KL10 mixing     |
C     *==========================================================*
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "KL10.h"

C     == Routine arguments ==
C     bi, bj :: tile numbers
C     iMin, iMax, jMin, jMax :: Range of points for which calculation
C     maskUp :: land/water mask for Wvel points (above tracer level)
C     myThid :: Instance number for this innvocation of KL10_CALC_VISC
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,k
      _RL KappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

#ifdef ALLOW_KL10

C     == Local variables ==
C     i, j    :: Loop counters
      INTEGER i,j
CEOP
      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRU(i,j,k) = max ( KappaRU(i,j,k),
     &         KappaRU(i,j,k) - viscArNr(k) + _maskW(i,j,k,bi,bj) *
     &         0.5*(KLviscAr(i,j,k,bi,bj)+KLviscAr(i-1,j,k,bi,bj)) )
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRV(i,j,k) = max ( KappaRV(i,j,k),
     &         KappaRV(i,j,k) - viscArNr(k) + _maskS(i,j,k,bi,bj) *
     &         0.5*(KLviscAr(i,j,k,bi,bj)+KLviscAr(i,j-1,k,bi,bj)) )
       ENDDO
      ENDDO

#endif /* ALLOW_KL10 */

      RETURN
      END
