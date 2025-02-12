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
C     | o Also update etaN if exactConserv=T
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

#ifdef EXACT_CONSERV
C     !LOCAL VARIABLES:
C     i,j,bi,bj  :: Loop counters
      INTEGER i,j,bi,bj
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        IF ( myIter.NE.nIter0 ) THEN
C--   Update etaN at the end of the time step :
C     Incorporate the Implicit part of -Divergence(Barotropic_Flow)

         IF ( implicDiv2DFlow.EQ.zeroRL ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            etaN(i,j,bi,bj) = etaH(i,j,bi,bj)
           ENDDO
          ENDDO
         ELSE
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            etaN(i,j,bi,bj) = etaH(i,j,bi,bj)
     &        + implicDiv2DFlow*dEtaHdt(i,j,bi,bj)*deltaTFreeSurf
           ENDDO
          ENDDO
         ENDIF

#ifdef ALLOW_OBCS
C--    Was added on Dec 30, 2004 (to fix unrealistic etaN ?), but no longer
C      needed with proper masking in solver (matrix+cg2d_b,x) and masking
C      of dEtaHdt above. etaN next to OB does not enter present algorithm but
C      leave it commented out in case we would implement an aternative scheme.
c      IF ( useOBCS ) CALL OBCS_APPLY_ETA( bi, bj, etaN, myThid )
#endif /* ALLOW_OBCS */

C-- end if exactConserv and not myIter=nIter0 block
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   before updating etaH, save current etaH field in etaHnm1
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            etaHnm1(i,j,bi,bj) = etaH(i,j,bi,bj)
          ENDDO
        ENDDO

C--   Update etaH at the end of the time step :
C     Incorporate the Explicit part of -Divergence(Barotropic_Flow)

        IF ( implicDiv2DFlow.EQ.oneRL ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            etaH(i,j,bi,bj) = etaN(i,j,bi,bj)
          ENDDO
         ENDDO
        ELSE
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            etaH(i,j,bi,bj) = etaN(i,j,bi,bj)
     &        + (oneRL - implicDiv2DFlow)*dEtaHdt(i,j,bi,bj)
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

      IF ( useOBCS.AND.nonlinFreeSurf.GT.0 )
     &    CALL EXCH_XY_RL( etaH, myThid )

#endif /* EXACT_CONSERV */

      RETURN
      END
