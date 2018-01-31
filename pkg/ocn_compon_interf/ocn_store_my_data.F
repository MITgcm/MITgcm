#include "OCN_CPL_OPTIONS.h"

C     !ROUTINE: OCN_STORE_MY_DATA
C     !INTERFACE:
      SUBROUTINE OCN_STORE_MY_DATA(
     I                     myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OCN_STORE_MY_DATA                             |
C     | o Routine for controlling storage of coupling data to    |
C     |   to the coupling layer.                                 |
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT   |
C     | Coupler "checkpoint1" library calls.                     |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "OCNCPL.h"
#ifdef ALLOW_THSICE
# include "THSICE_VARS.h"
#endif
c#ifdef ALLOW_DIC
c# include "DIC_VARS.h"
c#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myTime     :: Current time in simulation
C     myIter     :: Current timestep number
C     myThid     :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !FUNCTIONS:
c     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj      :: Current tile indices
C     i, j       :: Loop indices
      INTEGER bi,bj
      INTEGER i, j

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
C--   bi,bj loop over my tiles

         DO j=1,sNy
          DO i=1,sNx
            SSTocn2cpl(i,j,bi,bj) = theta(i,j,1,bi,bj)
            SSSocn2cpl(i,j,bi,bj) = salt (i,j,1,bi,bj)
          ENDDO
         ENDDO
#ifdef NONLIN_FRSURF
        IF ( nonlinFreeSurf.GT.0 .AND. staggerTimeStep ) THEN
         IF ( select_rStar.GT.0 ) THEN
          DO j=1,sNy
           DO i=1,sNx
            ocMxlD2cpl(i,j,bi,bj) =
     &           h0FacC(i,j,1,bi,bj)*rStarFacC(i,j,bi,bj)*drF(1)
           ENDDO
          ENDDO
         ELSE
          DO j=1,sNy
           DO i=1,sNx
            ocMxlD2cpl(i,j,bi,bj) = hFac_surfC(i,j,bi,bj)*drF(1)
           ENDDO
          ENDDO
         ENDIF
        ELSE
#else /* NONLIN_FRSURF */
        IF (.TRUE.) THEN
#endif /* NONLIN_FRSURF */
         DO j=1,sNy
          DO i=1,sNx
            ocMxlD2cpl(i,j,bi,bj) = hFacC(i,j,1,bi,bj)*drF(1)
          ENDDO
         ENDDO
        ENDIF

         DO j=1,sNy
          DO i=1,sNx
            vSqocn2cpl(i,j,bi,bj) =
     &              ( uVel(i , j,1,bi,bj)*uVel(i , j,1,bi,bj)
     &              + uVel(i+1,j,1,bi,bj)*uVel(i+1,j,1,bi,bj)
     &              + vVel(i,j+1,1,bi,bj)*vVel(i,j+1,1,bi,bj)
     &              + vVel(i,j , 1,bi,bj)*vVel(i,j , 1,bi,bj)
     &              )*0.5 _d 0
          ENDDO
         ENDDO

#ifdef ALLOW_THSICE
        IF ( useThSIce .AND. ocn_cplExch2W_sIce  ) THEN
         DO j=1,sNy
          DO i=1,sNx
            sIceFrac_cpl (i,j,bi,bj) = iceMask(i,j,bi,bj)
            sIceThick_cpl(i,j,bi,bj) = iceHeight(i,j,bi,bj)
            sIceSnowH_cpl(i,j,bi,bj) = snowHeight(i,j,bi,bj)
            sIceQ1_cpl   (i,j,bi,bj) = Qice1(i,j,bi,bj)
            sIceQ2_cpl   (i,j,bi,bj) = Qice2(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_THSICE */

c#ifdef ALLOW_DIC
c       IF ( ocn_cplExch_DIC ) THEN
c        DO j=1,sNy
c         DO i=1,sNx
c           fluxCO2cpl(i,j,bi,bj) = fluxCO2(i,j,bi,bj)
c         ENDDO
c        ENDDO
c       ENDIF
c#endif

C--   end bi,bj loops
       ENDDO
      ENDDO

      RETURN
      END
