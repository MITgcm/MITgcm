#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: POST_CG3D
C     !INTERFACE:
      SUBROUTINE POST_CG3D(
     I                      myTime, myIter, myThid )

C     !DESCRIPTION:
C     Called from SOLVE_FOR_PRESSURE, after 3-D solver (cg3d):
C     Finish computation of Non-hydrostatic pressure from 3-D solver solution

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#ifdef ALLOW_NONHYDROSTATIC
# include "PARAMS.h"
# include "GRID.h"
# include "SURFACE.h"
c#include "FFIELDS.h"
# include "DYNVARS.h"
# include "NH_VARS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myTime      :: Current time in simulation
C     myIter      :: Current iteration number in simulation
C     myThid      :: My Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_NONHYDROSTATIC
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
      INTEGER i, j, bi, bj
      INTEGER ks
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL     locGamma
CEOP

C--   Separate the Hydrostatic Surface Pressure adjusment (=> put it in dPhiNH)
C     from the Non-hydrostatic pressure (since cg3d_x contains both contribution)
      IF ( nonHydrostatic .AND. exactConserv ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

         IF ( selectNHfreeSurf.GE.1 ) THEN
          DO j=1,sNy
           DO i=1,sNx
            locGamma = drC(1)*recip_Bo(i,j,bi,bj)
     &               /( deltaTMom*deltaTFreeSurf
     &                 *implicitNHPress*implicDiv2DFlow )
            ks = 1
c           ks = kSurfC(i,j,bi,bj)
c           IF ( ks.LE.Nr ) THEN
             dPhiNH(i,j,bi,bj) = ( phi_nh(i,j,ks,bi,bj)
     &           + locGamma*Bo_surf(i,j,bi,bj)
     &                     *implicDiv2DFlow*deltaTFreeSurf
c    &                     *( wVel(i,j,ks,bi,bj) - wSurfP2d(i,j) )
     &                     *( wVel(i,j,ks,bi,bj) - dPhiNH(i,j,bi,bj) )
     &                           )/(1. _d 0 + locGamma )
c           ENDIF
           ENDDO
          ENDDO
         ELSEIF ( uniformFreeSurfLev ) THEN
C-       Z coordinate: assume surface @ level k=1
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             dPhiNH(i,j,bi,bj) = phi_nh(i,j,1,bi,bj)
           ENDDO
          ENDDO
         ELSE
C-       Other than Z coordinate: no assumption on surface level index
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            ks = kSurfC(i,j,bi,bj)
            IF ( ks.LE.Nr ) THEN
             dPhiNH(i,j,bi,bj) = phi_nh(i,j,ks,bi,bj)
            ELSE
             dPhiNH(i,j,bi,bj) = 0.
            ENDIF
           ENDDO
          ENDDO
         ENDIF

        ENDDO
       ENDDO
       IF ( selectNHfreeSurf.GE.1 .AND.
     &  ( implicitNHPress.LT.oneRL .OR. selectP_inEOS_Zc.EQ.3 ) ) THEN
         CALL EXCH_XY_RL( dPhiNH, myThid )
       ENDIF
      ENDIF

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
