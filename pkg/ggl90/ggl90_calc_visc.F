C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_calc_visc.F,v 1.6 2010/08/06 18:37:05 gforget Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

      SUBROUTINE GGL90_CALC_VISC(
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     U        KappaRU,KappaRV,
     I        myThid)

CBOP
C     *==========================================================*
C     | SUBROUTINE GGL90_CALC_VISC                               |
C     | o Add contrubution to net viscosity from GGL90 mixing    |
C     *==========================================================*
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "GGL90.h"

C     == Routine arguments ==
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C     myThid - Instance number for this innvocation of GGL90_CALC_VISC
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL KappaRU(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRV(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid
CEOP

#ifdef ALLOW_GGL90

C     == Local variables ==
C     I, J, K - Loop counters
      INTEGER i,j
      _RL p4, p8, p16

      p4=0.25 _d 0
      p8=0.125 _d 0
      p16=0.0625 _d 0

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRU(i,j,k) = KappaRU(i,j,k) +
     &                   (GGL90viscArU(i,j,k,bi,bj) - viscArNr(k) )
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRV(i,j,k) = KappaRV(i,j,k) + _maskS(i,j,k,bi,bj) *
     &                   (GGL90viscArV(i,j,k,bi,bj) - viscArNr(k) )
       ENDDO
      ENDDO

#endif /* ALLOW_GGL90 */

      RETURN
      END
