C $Header: /u/gcmpack/MITgcm/pkg/my82/my82_calc_diff.F,v 1.1 2004/09/02 09:11:54 mlosch Exp $
C $Name:  $

#include "MY82_OPTIONS.h"

      SUBROUTINE MY82_CALC_DIFF( 
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     I        maskUp,
     U        KappaRT,KappaRS,
     I        myThid)

C     /==========================================================\
C     | SUBROUTINE MY82_CALC_DIFF                                 |
C     | o Add contrubution to net diffusivity from MY82 mixing    |
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
C     maskUp  - land/water mask for Wvel points (above tracer level)
C     myThid - Instance number for this innvocation of CALC_COMMON_FACTORS
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RS maskUp(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL KappaRT(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRS(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid

#ifdef ALLOW_MY82
#include "MY82.h"

C     == Local variables ==
C     I, J, K - Loop counters
      INTEGER i,j

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRT(i,j,k) = KappaRT(i,j,k)+maskUp(i,j)*
     &                   ( MYdiffKr(i,j,k,bi,bj)
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL))
     &                     - diffKr(i,j,k,bi,bj) )
#else
     &                     - diffKrT )
#endif
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRS(i,j,k) = KappaRS(i,j,k)+maskUp(i,j)*
     &                   ( MYdiffKr(i,j,k,bi,bj)
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL))
     &                     - diffKr(i,j,k,bi,bj) )
#else
     &                     - diffKrS )
#endif
       ENDDO
      ENDDO

#endif /* ALLOW_MY82 */

      RETURN
      END
