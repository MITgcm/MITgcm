C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_import_ocnconfig.F,v 1.2 2004/05/21 20:00:48 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_IMPORT_OCNCONFIG( myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_IMPORT_OCNCONFIG                          |
C     | o Routine for importing ocean config into atmos.         |
C     |   component.                                             |
C     |==========================================================|
C     | This version talks to the MIT Coupler. It uses the       |
C     | MIT Coupler "checkpoint 1" library calls.                |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATMIDS.h"
#include "ATMCPL.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine
      INTEGER myThid
CEndOfInterface

#ifdef COMPONENT_MODULE
C     == Local variables ==
C     i,j,bi,bj - Loop counters
      INTEGER i,j,bi,bj

C-    Initialise mixed-layer depth
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myByLo(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          ocMxlD (i,j,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     Receive ocean model configuration info.
C     o Import ocean model mixed-layer depth
      CALL COMPRECV_R8TILES( atmMxlDName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O     ocMxlD )

#endif /* COMPONENT_MODULE */

      RETURN
      END
