#include "ATM_CPL_OPTIONS.h"

CBOP 0
C !ROUTINE: ATM_IMPORT_OCNCONFIG

C !INTERFACE:
      SUBROUTINE ATM_IMPORT_OCNCONFIG( myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE ATM_IMPORT_OCNCONFIG
C     | o Routine for importing ocean config
C     |   into atmos. component.
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the
C     | MIT Coupler "checkpoint 1" library calls.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATMIDS.h"
#include "ATMCPL.h"

C !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Thread number for this instance of the routine
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C !LOCAL VARIABLES:
C     == Local variables ==
C     i,j,bi,bj :: Loop counters
      INTEGER i,j,bi,bj

C-    Initialise mixed-layer depth
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          ocMxlD (i,j,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C-    Receive ocean model configuration info.
      _BARRIER
      _BEGIN_MASTER( myThid )

C     o Import ocean model mixed-layer depth
      CALL COMPRECV_R8TILES(
     I              atmMxlDName, sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O              ocMxlD )

      _END_MASTER( myThid )
      _BARRIER

#endif /* COMPONENT_MODULE */

      RETURN
      END
