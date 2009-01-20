C $Header: /u/gcmpack/MITgcm/pkg/my82/my82_calc_diff.F,v 1.4 2009/01/20 00:26:04 jmc Exp $
C $Name:  $

#include "MY82_OPTIONS.h"

      SUBROUTINE MY82_CALC_DIFF(
     I        bi,bj,iMin,iMax,jMin,jMax,kArg,kSize,
     U        KappaRx,
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
#include "GRID.h"
#ifdef ALLOW_3D_DIFFKR
#include "DYNVARS.h"
#endif
#include "MY82.h"

C     == Routine arguments ==
C     bi, bj,   :: tile indices
C     iMin,iMax :: Range of points for which calculation is done
C     jMin,jMax :: Range of points for which calculation is done
C     kArg      :: = 0 -> do the k-loop here and treat all levels
C                  > 0 -> k-loop is done outside and treat only level k=kArg
C     kSize     :: 3rd Dimension of the vertical diffusivity array KappaRx
C     KappaRx   :: vertical diffusivity array
C     myThid    :: Instance number for this innvocation of MY82_CALC_DIFF

      INTEGER bi,bj,iMin,iMax,jMin,jMax,kArg,kSize
      _RL KappaRx(1-Olx:sNx+Olx,1-Oly:sNy+Oly,kSize)
      INTEGER myThid

#ifdef ALLOW_MY82

C     == Local variables ==
C     i,j,k     :: Loop counters
      INTEGER i,j,k

C--   Add Vertical diffusivity contribution from MY82
      IF ( kArg .EQ. 0 ) THEN
C-    do all levels :
        DO k=1,MIN(Nr,kSize)
         DO j=jMin,jMax
          DO i=iMin,iMax
            KappaRx(i,j,k) = KappaRx(i,j,k)
     &                  +( MYdiffKr(i,j,k,bi,bj)
#ifdef ALLOW_3D_DIFFKR
     &                     - diffKr(i,j,k,bi,bj) )
#else
     &                     - diffKrNrT(k) )
#endif
          ENDDO
         ENDDO
        ENDDO
      ELSE
C-    do level k=kArg only :
         k = MIN(kArg,kSize)
         DO j=jMin,jMax
          DO i=iMin,iMax
            KappaRx(i,j,k) = KappaRx(i,j,k)
     &                  +( MYdiffKr(i,j,kArg,bi,bj)
#ifdef ALLOW_3D_DIFFKR
     &                     - diffKr(i,j,kArg,bi,bj) )
#else
     &                     - diffKrNrT(kArg) )
#endif
          ENDDO
         ENDDO
      ENDIF

#endif /* ALLOW_MY82 */

      RETURN
      END
