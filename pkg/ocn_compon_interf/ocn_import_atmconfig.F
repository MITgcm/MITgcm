#include "OCN_CPL_OPTIONS.h"

CBOP 0
C !ROUTINE: OCN_IMPORT_ATMCONFIG

C !INTERFACE:
      SUBROUTINE OCN_IMPORT_ATMCONFIG( myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OCN_IMPORT_ATMCONFIG
C     | o Routine for importing atmos. config
C     |   into ocean component.
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
#include "OCNCPL.h"

C !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Thread number for this instance of the routine
      INTEGER myThid
CEOP

C !LOCAL VARIABLES:
C     == Local variables ==
C     i,j,bi,bj :: Loop counters
      INTEGER i,j,bi,bj

C-    Initialise land-mask
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          landMask(i,j,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C-    Receive atmos. model configuration info.
      _BARRIER
      _BEGIN_MASTER( myThid )

C     o Import atmosphere model land-mask
      CALL COMPRECV_R8TILES(
     I              atmLandName, sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O              landMask )

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END
