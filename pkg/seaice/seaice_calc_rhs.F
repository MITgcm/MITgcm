C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_calc_rhs.F,v 1.7 2013/04/04 07:02:51 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CBOP
C     !ROUTINE: SEAICE_CALC_RHS
C     !INTERFACE:
      SUBROUTINE SEAICE_CALC_RHS(
     O     uIceRHS, vIceRHS,
     I     newtonIter, krylovIter, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_CALC_RHS
C     | o Right-hand side of momentum equations, i.e. all terms
C     |   that do not depend on the ice velocities of the current
C     |   iterate of the Newton-Krylov iteration
C     *==========================================================*
C     | written by Martin Losch, Oct 2012
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
C     newtonIter :: current iterate of Newton iteration
C     krylovIter :: current iterate of Krylov iteration
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER newtonIter
      INTEGER krylovIter
C     u/vIceRHS :: RHS of momentum equations
      _RL uIceRHS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceRHS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef SEAICE_ALLOW_JFNK
C     i,j,bi,bj,kSrf :: loop indices
      INTEGER i,j,bi,bj
      INTEGER kSrf
      _RS     SINWAT
      _RL     COSWAT
CEOP

      kSrf=1
C--   introduce turning angles
      SINWAT=SIN(SEAICE_waterTurnAngle*deg2rad)
      COSWAT=COS(SEAICE_waterTurnAngle*deg2rad)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1,sNy
         DO I=1,sNx
C     ice-velocity independent contribution to drag terms
C     - Cdrag*(uVel*cosWat - vVel*sinWat)/(vVel*cosWat + uVel*sinWat)
C     ( remember to average to correct velocity points )
          uIceRHS(I,J,bi,bj) = FORCEX(I,J,bi,bj) + (
     &         0.5 _d 0 * ( DWATN(I,J,bi,bj)+DWATN(I-1,J,bi,bj) ) *
     &         COSWAT * uVel(I,J,kSrf,bi,bj)
     &         - SIGN(SINWAT, _fCori(I,J,bi,bj))* 0.5 _d 0 *
     &         ( DWATN(I  ,J,bi,bj) * 0.5 _d 0 *
     &          ( vVel(I  ,J,kSrf,bi,bj)+vVel(I  ,J+1,kSrf,bi,bj) )
     &         + DWATN(I-1,J,bi,bj) * 0.5 _d 0 *
     &          ( vVel(I-1,J,kSrf,bi,bj)+vVel(I-1,J+1,kSrf,bi,bj) )
     &         ) )
          vIceRHS(I,J,bi,bj) = FORCEY(I,J,bi,bj) + (
     &         0.5 _d 0 * ( DWATN(I,J,bi,bj)+DWATN(I,J-1,bi,bj) ) *
     &         COSWAT * vVel(I,J,kSrf,bi,bj)
     &         + SIGN(SINWAT, _fCori(I,J,bi,bj)) * 0.5 _d 0 *
     &         ( DWATN(I,J  ,bi,bj) * 0.5 _d 0 *
     &          ( uVel(I,J  ,kSrf,bi,bj)+uVel(I+1,J  ,kSrf,bi,bj))
     &         + DWATN(I,J-1,bi,bj) * 0.5 _d 0 *
     &          ( uVel(I,J-1,kSrf,bi,bj)+uVel(I+1,J-1,kSrf,bi,bj))
     &         ) )
C     apply masks for interior (important when we have open boundaries)
          uIceRHS(I,J,bi,bj) = uIceRHS(I,J,bi,bj)*maskinW(I,J,bi,bj)
          vIceRHS(I,J,bi,bj) = vIceRHS(I,J,bi,bj)*maskinS(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_JFNK */

      RETURN
      END
