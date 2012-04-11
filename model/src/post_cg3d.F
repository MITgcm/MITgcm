C $Header: /u/gcmpack/MITgcm/model/src/post_cg3d.F,v 1.4 2012/04/11 15:49:38 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: POST_CG3D
C     !INTERFACE:
      SUBROUTINE POST_CG3D(
     I                      zeroPsNH, zeroMeanPnh,
     I                      myTime, myIter, myThid )

C     !DESCRIPTION:
C     Called from SOLVE_FOR_PRESSURE, after 3-D solver (cg3d):
C     Finish computation of Non-hydrostatic pressure from 3-D solver solution

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
c#include "FFIELDS.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     zeroPsNH    :: account for Hyd.component of cg3d_x by updating NH & Surf.Press
C     zeroMeanPnh :: account for Hyd.component of cg3d_x by updating NH & Surf.Press
C     myTime      :: Current time in simulation
C     myIter      :: Current iteration number in simulation
C     myThid      :: My Thread Id. number
      LOGICAL zeroPsNH, zeroMeanPnh
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_NONHYDROSTATIC
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k,bi,bj
      INTEGER ks
      CHARACTER*10 sufx
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL     tmpSurf
CEOP

C--   Separate the Hydrostatic Surface Pressure adjusment (=> put it in dPhiNH)
C     from the Non-hydrostatic pressure (since cg3d_x contains both contribution)
      IF ( nonHydrostatic .AND. exactConserv ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         IF ( select_rStar.EQ.0 .AND. uniformFreeSurfLev ) THEN
C-       Z coordinate: assume surface @ level k=1
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             dPhiNH(i,j,bi,bj) = phi_nh(i,j,1,bi,bj)
           ENDDO
          ENDDO
         ELSEIF ( select_rStar.EQ.0 ) THEN
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
#ifdef NONLIN_FRSURF
         ELSE
C        rStar : take vertical average of P_NH as Hyd.Surf.Press adjustment
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             dPhiNH(i,j,bi,bj) = 0.
           ENDDO
          ENDDO
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             dPhiNH(i,j,bi,bj) = dPhiNH(i,j,bi,bj)
     &         + phi_nh(i,j,k,bi,bj)*drF(k)*h0FacC(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             dPhiNH(i,j,bi,bj) = dPhiNH(i,j,bi,bj)*recip_Rcol(i,j,bi,bj)
           ENDDO
          ENDDO
#endif /* NONLIN_FRSURF */
         ENDIF
         IF ( selectNHfreeSurf.GE.1 ) THEN
          DO j=1,sNy
           DO i=1,sNx
             tmpSurf = deltaTMom*deltaTfreesurf
     &                *Bo_surf(i,j,bi,bj)*recip_drC(1)
     &                *implicitNHPress*implicDiv2DFlow
             dPhiNH(i,j,bi,bj) = ( tmpSurf*dPhiNH(i,j,bi,bj)
     &           + Bo_surf(i,j,bi,bj)*
     &                  ( etaH(i,j,bi,bj)-etaN(i,j,bi,bj)
     &                   +implicDiv2DFlow*deltaTfreesurf
c    &                                   *(wVel(i,j,1,bi,bj)+PmE)
     &                                   *wVel(i,j,1,bi,bj)
     &                  )        )/(1. _d 0 + tmpSurf )
           ENDDO
          ENDDO
         ENDIF
        ENDDO
       ENDDO
       IF ( selectNHfreeSurf.GE.1 .AND.
     &      implicitNHPress.LT.1. _d 0 ) THEN
         CALL EXCH_XY_RL( dPhiNH, myThid )
       ENDIF
      ENDIF

C--   Update surface pressure (account for NH-p @ surface level) and NH pressure:
      IF ( zeroPsNH .OR. zeroMeanPnh ) THEN
       IF ( DIFFERENT_MULTIPLE( diagFreq, myTime, deltaTClock) ) THEN
        WRITE(sufx,'(I10.10)') myIter
        CALL WRITE_FLD_XYZ_RL( 'cg3d_x.',sufx, phi_nh, myIter, myThid )
       ENDIF
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            phi_nh(i,j,k,bi,bj) = ( phi_nh(i,j,k,bi,bj)
     &                            - dPhiNH(i,j,bi,bj)
     &                            )*maskC(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            etaN(i,j,bi,bj) = etaN(i,j,bi,bj)
     &                      + recip_Bo(i,j,bi,bj)*dPhiNH(i,j,bi,bj)
          ENDDO
         ENDDO

        ENDDO
       ENDDO
      ENDIF
#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
