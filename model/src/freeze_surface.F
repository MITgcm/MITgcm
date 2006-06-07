C $Header: /u/gcmpack/MITgcm/model/src/freeze_surface.F,v 1.7 2006/06/07 01:55:13 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: FREEZE_SURFACE
C     !INTERFACE:
      SUBROUTINE FREEZE_SURFACE( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R FREEZE_SURFACE                                                
C     | o Check water temperature and limit range of temperature  
C     | appropriately.                                            
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid :: Thread no. that called this routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     Tfreezing :: Freezing threshold temperature.
      INTEGER bi,bj,i,j,k
      _RL Tfreezing
CEOP

      IF ( usingPCoords ) THEN
        k = Nr
      ELSE
        k = 1
      ENDIF

      Tfreezing = -1.9 _d 0

C     Check for water that should have frozen
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF (theta(i,j,k,bi,bj) .LT. Tfreezing) THEN
             surfaceForcingTice(i,j,bi,bj) =
     &            ( Tfreezing - theta(i,j,k,bi,bj) )
     &                    *drF(k)*_hFacC(i,j,k,bi,bj) / dTtracerLev(k)
             theta(i,j,k,bi,bj) = Tfreezing
          ELSE
             surfaceForcingTice(i,j,bi,bj) = 0. _d 0
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
