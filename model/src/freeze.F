C $Header: /u/gcmpack/MITgcm/model/src/Attic/freeze.F,v 1.9 2003/11/13 06:35:14 dimitri Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: FREEZE
C     !INTERFACE:
      SUBROUTINE FREEZE( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R FREEZE                                                
C     | o Check water temperature and limit range of temperature  
C     | appropriately.                                            
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     Tfreezing :: Freezing threshold temperature.
      INTEGER bi,bj,i,j
      _RL Tfreezing
CEOP

      Tfreezing=-1.9

C     Check for water that should have frozen
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          surfacetendencyTice(I,J,bi,bj) = 0. _d 0
          IF (theta(I,J,1,bi,bj) .LT. Tfreezing) THEN
             surfaceTendencyTice(I,J,bi,bj) =
     &            ( Tfreezing - theta(I,J,1,bi,bj) ) / deltaTtracer
             theta(I,J,1,bi,bj) = Tfreezing
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
