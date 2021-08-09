#include "SEAICE_OPTIONS.h"

CBOP
C     !ROUTINE: SEAICE_DIFFUSION
C     !INTERFACE:
      SUBROUTINE SEAICE_DIFFUSION(
     I     tracerIdentity,
     I     diffKh, fac,
     I     iceFld, iceMask, xA, yA,
     U     gFld,
     I     bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_DIFFUSION
C     | o Add tendency from horizontal diffusion
C     *==========================================================*
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
CML#include "SEAICE_GRID.h"

C     !INPUT PARAMETERS:
C     === Routine arguments ===
C     afx        :: horizontal advective flux, x direction
C     afy        :: horizontal advective flux, y direction
C     myThid     :: my Thread Id number
      _RL iceFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL iceMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS xA     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL diffKh, fac
      INTEGER tracerIdentity
      INTEGER bi,bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
#ifdef ALLOW_GENERIC_ADVDIFF
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 SEAICE_DIAG_SUFX, diagSufx
      EXTERNAL    SEAICE_DIAG_SUFX
#endif
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j :: Loop counters
      INTEGER i, j, k
      _RL fZon   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      IF ( diffKh .GT. 0. _d 0 ) THEN
#ifdef ALLOW_DIAGNOSTICS
C--   Set diagnostic suffix for the current tracer
       IF ( useDiagnostics ) THEN
        diagSufx = SEAICE_DIAG_SUFX( tracerIdentity, myThid )
       ENDIF
#endif
C--   Tendency due to horizontal diffusion
        k = 1
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fZon  (i,j) = 0. _d 0
          fMer  (i,j) = 0. _d 0
         ENDDO
        ENDDO
C--   X-direction
        CALL GAD_DIFF_X(bi,bj,k,xA,diffKh,iceFld,fZon,myThid)
C--   Y-direction
        CALL GAD_DIFF_Y(bi,bj,k,yA,diffKh,iceFld,fMer,myThid)
C--   Divergence of fluxes: update scalar field
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLx-1
          gFld(i,j)= gFld(i,j)
     &        - fac*iceMask(i,j,bi,bj)*recip_rA(i,j,bi,bj)
     &        *( (fZon(i+1,j)-fZon(i,j))
     &         + (fMer(i,j+1)-fMer(i,j)) )
         ENDDO
        ENDDO
#ifdef ALLOW_DIAGNOSTICS
C-    Diagnostics of Tracer flux in Y dir (mainly Diffusive terms),
C     excluding advective terms:
        IF ( useDiagnostics .AND. diffKh.NE.0. ) THEN
         diagName = 'DFxE'//diagSufx
         CALL DIAGNOSTICS_FILL(fZon,diagName, k,1, 2,bi,bj, myThid)
         diagName = 'DFyE'//diagSufx
         CALL DIAGNOSTICS_FILL(fMer,diagName, k,1, 2,bi,bj, myThid)
        ENDIF
#endif
C     endif do horizontal diffusion
      ENDIF
#endif /* ALLOW_GENERIC_ADVDIFF */

      RETURN
      END
