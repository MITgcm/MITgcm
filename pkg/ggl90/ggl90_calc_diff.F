#include "GGL90_OPTIONS.h"

CBOP
C     !ROUTINE: GGL90_CALC_DIFF

C     !INTERFACE:
      SUBROUTINE GGL90_CALC_DIFF(
     I        bi, bj, iMin, iMax, jMin, jMax, kArg, kSize,
     U        KappaRx,
     I        myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GGL90_CALC_DIFF                               |
C     | o Add contribution to net diffusivity from GGL90 mixing  |
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c#include "DYNVARS.h"
c#include "GRID.h"
#include "GGL90.h"

C     !INPUT/OUTPUT PARAMETERS:
C     bi, bj    :: current tile indices
C     iMin,iMax :: Range of points for which calculation is done
C     jMin,jMax :: Range of points for which calculation is done
C     kArg      :: = 0 -> do the k-loop here and treat all levels
C                  > 0 -> k-loop is done outside and treat only level k=kArg
C     kSize     :: 3rd Dimension of the vertical diffusivity array KappaRx
C     KappaRx   :: vertical diffusivity array
C     myThid    :: my Thread Id number
      INTEGER bi, bj, iMin, iMax, jMin, jMax, kArg, kSize
      _RL KappaRx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize)
      INTEGER myThid

#ifdef ALLOW_GGL90
C     !LOCAL VARIABLES:
C     i, j, k   :: Loop counters
      INTEGER i, j, k
CEOP

C--   Add Vertical diffusivity contribution from GGL90
      IF ( kArg .EQ. 0 ) THEN
C-    do all levels :
        DO k=1,MIN(Nr,kSize)
         DO j=jMin,jMax
          DO i=iMin,iMax
            KappaRx(i,j,k) = KappaRx(i,j,k)
     &                  +( GGL90diffKr(i,j,k,bi,bj)
     &                     - diffKrNrS(k) )
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
     &                     - diffKrNrS(kArg) )
          ENDDO
         ENDDO
      ENDIF

#endif /* ALLOW_GGL90 */

      RETURN
      END
