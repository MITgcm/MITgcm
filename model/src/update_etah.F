#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: UPDATE_ETAH
C     !INTERFACE:
      SUBROUTINE UPDATE_ETAH( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE UPDATE_ETAH
C     | o Update etaH after mom-correction-step/integr_continuity
C     |  (required with NLFS to derive surface layer thickness)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number
C     myThid  :: my Thread Id number
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     i,j,bi,bj  :: Loop counters
      INTEGER i,j,bi,bj
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C--   before updating etaH, save current etaH field in etaHnm1
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            etaHnm1(i,j,bi,bj) = etaH(i,j,bi,bj)
          ENDDO
        ENDDO

C--   Update etaH at the end of the time step :
C     Incorporate the Explicit part of -Divergence(Barotropic_Flow)

        IF (implicDiv2Dflow.EQ. 1. _d 0) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            etaH(i,j,bi,bj) = etaN(i,j,bi,bj)
          ENDDO
         ENDDO

        ELSE
         DO j=1,sNy
          DO i=1,sNx
            etaH(i,j,bi,bj) = etaN(i,j,bi,bj)
     &       + (1. - implicDiv2Dflow)*dEtaHdt(i,j,bi,bj)
     &                               *deltaTFreeSurf
          ENDDO
         ENDDO
        ENDIF

#ifdef ALLOW_OBCS
C--    Apply OBC to etaH (NonLin-FreeSurf): needed since viscous terms
C       depend on hFacZ which is not only function of boundary hFac values.
        IF ( useOBCS.AND.nonlinFreeSurf.GT.0 )
     &     CALL OBCS_APPLY_ETA( bi, bj, etaH, myThid )
#endif /* ALLOW_OBCS */

C- end bi,bj loop.
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( implicDiv2Dflow .NE. 1. _d 0 .OR.
     &    ( useOBCS.AND.nonlinFreeSurf.GT.0 ) )
     &    CALL EXCH_XY_RL( etaH, myThid )

      RETURN
      END
