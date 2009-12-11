C $Header: /u/gcmpack/MITgcm/model/src/post_cg3d.F,v 1.1 2009/12/11 04:31:31 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
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
c     LOGICAL  DIFFERENT_MULTIPLE
c     EXTERNAL DIFFERENT_MULTIPLE

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
c     INTEGER ioUnit
c     CHARACTER*10 sufx
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL     tmpVar(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
CEOP

c     IF ( use3Dsolver ) THEN
C--   Update surface pressure (account for NH-p @ surface level) and NH pressure:
      IF ( zeroPsNH .OR. zeroMeanPnh ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

         IF ( zeroPsNH .AND. usingZCoords ) THEN
C-       Z coordinate: assume surface @ level k=1
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             tmpVar(i,j) = phi_nh(i,j,1,bi,bj)
           ENDDO
          ENDDO
         ELSEIF ( zeroPsNH ) THEN
C-       Other than Z coordinate: no assumption on surface level index
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            ks = ksurfC(i,j,bi,bj)
            IF ( ks.LE.Nr ) THEN
             tmpVar(i,j) = phi_nh(i,j,ks,bi,bj)
            ELSE
             tmpVar(i,j) = 0.
            ENDIF
           ENDDO
          ENDDO
#ifdef NONLIN_FRSURF
         ELSE
C        zeroMeanPnh : transfert vertical average of P_NH to EtaN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             tmpVar(i,j) = 0.
           ENDDO
          ENDDO
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             tmpVar(i,j) = tmpVar(i,j)
     &         + phi_nh(i,j,k,bi,bj)*drF(k)*h0FacC(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             tmpVar(i,j) = tmpVar(i,j)*recip_Rcol(i,j,bi,bj)
           ENDDO
          ENDDO
#endif /* NONLIN_FRSURF */
         ENDIF
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            phi_nh(i,j,k,bi,bj) = ( phi_nh(i,j,k,bi,bj)
     &                            - tmpVar(i,j)
     &                            )*maskC(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            etaN(i,j,bi,bj) = etaN(i,j,bi,bj)
     &                      + recip_Bo(i,j,bi,bj)*tmpVar(i,j)
          ENDDO
         ENDDO

        ENDDO
       ENDDO
      ENDIF

c     ENDIF
#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END
