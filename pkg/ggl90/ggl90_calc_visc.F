#include "GGL90_OPTIONS.h"

CBOP
C     !ROUTINE: GGL90_CALC_VISC

C     !INTERFACE:
      SUBROUTINE GGL90_CALC_VISC(
     I        bi, bj, iMin, iMax, jMin, jMax, k,
     U        KappaRU, KappaRV,
     I        myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GGL90_CALC_VISC                               |
C     | o Add contribution to net viscosity from GGL90 mixing    |
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
C     k         :: current level index
C     KappaRU   :: vertical viscosity array for U-component
C     KappaRV   :: vertical viscosity array for V-component
C     myThid    :: my Thread Id number
      INTEGER bi, bj, iMin, iMax, jMin, jMax, k
      _RL KappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

#ifdef ALLOW_GGL90
C     !LOCAL VARIABLES:
C     i, j      :: Loop counters
      INTEGER i, j
CEOP

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRU(i,j,k) = KappaRU(i,j,k)
     &                 + ( GGL90viscArU(i,j,k,bi,bj) - viscArNr(k) )
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
        KappaRV(i,j,k) = KappaRV(i,j,k)
     &                 + ( GGL90viscArV(i,j,k,bi,bj) - viscArNr(k) )
       ENDDO
      ENDDO

#endif /* ALLOW_GGL90 */

      RETURN
      END
