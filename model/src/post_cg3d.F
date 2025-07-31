#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: POST_CG3D
C     !INTERFACE:
      SUBROUTINE POST_CG3D(
     I                      cg3d_x, wFld, wSurfP2d,
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
# include "NH_VARS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     cg3d_x      :: Solution vector of the 3-D solver equation A.X=B
C     wFld        :: Provisional vertical velocity
C     wSurfP2d    :: surface vertical velocity after 2-D solver
C     myTime      :: Current time in simulation
C     myIter      :: Current iteration number in simulation
C     myThid      :: My Thread Id. number
      _RL     cg3d_x  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     wFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     wSurfP2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_NONHYDROSTATIC
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
      INTEGER i, j, bi, bj
      INTEGER k, ks
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
             dPhiNH(i,j,bi,bj) =
     &            cg3d_x(i,j,ks,bi,bj)/(1. _d 0 + locGamma )
     &          + locGamma*Bo_surf(i,j,bi,bj)
     &                    *implicDiv2DFlow*deltaTFreeSurf
     &                    *( wFld(i,j,ks,bi,bj) - wSurfP2d(i,j,bi,bj) )
c           ENDIF
           ENDDO
          ENDDO
         ELSEIF ( uniformFreeSurfLev ) THEN
C-       Z coordinate: assume surface @ level k=1
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             dPhiNH(i,j,bi,bj) = cg3d_x(i,j,1,bi,bj)
           ENDDO
          ENDDO
         ELSE
C-       Other than Z coordinate: no assumption on surface level index
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            ks = kSurfC(i,j,bi,bj)
            IF ( ks.LE.Nr ) THEN
             dPhiNH(i,j,bi,bj) = cg3d_x(i,j,ks,bi,bj)
            ELSE
             dPhiNH(i,j,bi,bj) = 0.
            ENDIF
           ENDDO
          ENDDO
         ENDIF

        ENDDO
       ENDDO
       IF ( selectNHfreeSurf.GE.1 .AND.
     &  ( solveForDeltaP .OR. implicitNHPress.LT.oneRL
     &                   .OR. selectP_inEOS_Zc.EQ.3 ) ) THEN
         CALL EXCH_XY_RL( dPhiNH, myThid )
       ENDIF
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        IF ( solveForDeltaP ) THEN
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              phi_nh(i,j,k,bi,bj) = phi_nh(i,j,k,bi,bj)
     &              + ( cg3d_x(i,j,k,bi,bj) - dPhiNH(i,j,bi,bj) )
     &                *maskC(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             dPhiNH(i,j,bi,bj) = 0.
           ENDDO
          ENDDO
        ELSE
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              phi_nh(i,j,k,bi,bj) = cg3d_x(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
        ENDIF
       ENDDO
      ENDDO

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
