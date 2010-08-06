C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_calc_diff.F,v 1.6 2010/08/06 18:37:05 gforget Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

      SUBROUTINE GGL90_CALC_DIFF( 
     I        bi,bj,iMin,iMax,jMin,jMax,kArg,kSize,
     U        KappaRx,
     I        myThid)

C     /==========================================================\
C     | SUBROUTINE GGL90_CALC_DIFF                               |
C     | o Add contrubution to net diffusivity from GGL90 mixing  |
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_GGL90
#include "GGL90.h"
#endif

C     == Routine arguments ==
C     bi, bj,   :: tile indices
C     iMin,iMax :: Range of points for which calculation is done
C     jMin,jMax :: Range of points for which calculation is done
C     kArg      :: = 0 -> do the k-loop here and treat all levels
C                  > 0 -> k-loop is done outside and treat only level k=kArg
C     kSize     :: 3rd Dimension of the vertical diffusivity array KappaRx
C     KappaRx   :: vertical diffusivity array
C     myThid    :: Instance number for this innvocation of GGL90_CALC_DIFF

      INTEGER bi,bj,iMin,iMax,jMin,jMax,kArg,kSize
      _RL KappaRx(1-Olx:sNx+Olx,1-Oly:sNy+Oly,kSize)
      INTEGER myThid

#ifdef ALLOW_GGL90

C     == Local variables ==
C     i,j,k     :: Loop counters
      INTEGER i,j,k

C--   Add Vertical diffusivity contribution from GGL90
      IF ( kArg .EQ. 0 ) THEN
C-    do all levels :
        DO k=1,MIN(Nr,kSize)
         DO j=jMin,jMax
          DO i=iMin,iMax
            KappaRx(i,j,k) = KappaRx(i,j,k)
     &                  +( GGL90diffKr(i,j,k,bi,bj)
     &                     - diffKrNrT(k) )
          ENDDO
         ENDDO
        ENDDO
      ELSE
C-    do level k=kArg only :
         k = MIN(kArg,kSize)
         DO j=jMin,jMax
          DO i=iMin,iMax
            KappaRx(i,j,k) = KappaRx(i,j,k)
     &                  +( GGL90diffKr(i,j,kArg,bi,bj)
     &                     - diffKrNrT(kArg) )
          ENDDO
         ENDDO
      ENDIF

#endif /* ALLOW_GGL90 */

      RETURN
      END
