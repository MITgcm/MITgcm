C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_calc_diff.F,v 1.4 2001/12/16 18:54:49 jmc Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

      SUBROUTINE GMREDI_CALC_DIFF( 
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     I        maskUp,
     U        KappaRT,KappaRS,
     I        myThid)

C     /==========================================================\
C     | SUBROUTINE GMREDI_CALC_DIFF                              |
C     | o Add contrubution to net diffusivity from GM/Redi       |
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C     maskUp  - land/water mask for Wvel points (above tracer level)
C     myThid - Instance number for this innvocation of CALC_COMMON_FACTORS
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RS maskUp(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL KappaRT(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRS(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid

#ifdef ALLOW_GMREDI
#include "GMREDI.h"

C     == Local variables ==
C     I, J   - Loop counters
      INTEGER i,j

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRT(i,j,k) = KappaRT(i,j,k)+maskUp(i,j)*Kwz(i,j,k,bi,bj)
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRS(i,j,k) = KappaRS(i,j,k)+maskUp(i,j)*Kwz(i,j,k,bi,bj)
       ENDDO
      ENDDO

#endif /* ALLOW_GMREDI */

      RETURN
      END
