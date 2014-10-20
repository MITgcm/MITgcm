C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_calc_lhs.F,v 1.9 2014/10/20 03:20:57 gforget Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_CALC_LHS
C     !INTERFACE:
      SUBROUTINE SEAICE_CALC_LHS(
     I     uIceLoc, vIceLoc,
     O     uIceLHS, vIceLHS,
     I     newtonIter, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_CALC_LHS
C     | o Left-hand side of momentum equations, i.e. all terms
C     |   that depend on the ice velocities of the current
C     |   iterate of the Newton-Krylov iteration
C     |
C     | o The scheme is backward Euler in time, i.e. the
C     |   rhs-vector contains only terms that are independent
C     |   of u/vIce, except for the time derivative part
C     |   mass*(u/vIce-u/vIceNm1)/deltaT
C     | o Left-hand side contributions
C     |   + mass*(u/vIce)/deltaT
C     |   + Cdrag*(uIce*cosWat - vIce*sinWat)
C     |          /(vIce*cosWat + uIce*sinWat)
C     |   - mass*f*vIce/+mass*f*uIce
C     |   - dsigma/dx / -dsigma/dy, eta and zeta are
C     |                   computed only once per Newton iterate
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
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
C     newtonIter :: current iterate of Newton iteration
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER newtonIter
C     u/vIceLoc :: local copies of the current ice velocity
      _RL uIceLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     u/vIceLHS :: LHS of momentum equations
      _RL uIceLHS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceLHS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef SEAICE_ALLOW_JFNK
C     i,j,bi,bj,k :: loop indices
      INTEGER i,j,bi,bj
      INTEGER k
      _RS     SINWAT
      _RL     COSWAT, recip_deltaT, eplus, eminus
C     backward difference extrapolation factor
      _RL bdfAlpha
C     components of symmetric stress tensor
      _RL sig11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig22(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

      k=1
      recip_deltaT = 1. _d 0 / SEAICE_deltaTdyn
C--   introduce turning angles
      SINWAT=SIN(SEAICE_waterTurnAngle*deg2rad)
      COSWAT=COS(SEAICE_waterTurnAngle*deg2rad)
C     backward difference extrapolation factor
      bdfAlpha = 1. _d 0
      IF ( SEAICEuseBDF2 ) THEN
       IF ( myIter.EQ.nIter0 .AND. SEAICEmomStartBDF.EQ.0 ) THEN
        bdfAlpha = 1. _d 0
       ELSE
        bdfAlpha = 1.5 _d 0
       ENDIF
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     compute components of stress tensor from current velocity field
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          sig11(I,J) = 0. _d 0
          sig22(I,J) = 0. _d 0
          sig12(I,J) = 0. _d 0
         ENDDO
        ENDDO

        DO j=0,sNy
         DO i=0,sNx
          eplus = e11(I,J,bi,bj) + e22(I,J,bi,bj)
          eminus= e11(I,J,bi,bj) - e22(I,J,bi,bj)
          sig11(I,J) = zeta(I,J,bi,bj)*eplus + eta(I,J,bi,bj)*eminus
     &         - 0.5 _d 0 * PRESS(I,J,bi,bj)
          sig22(I,J) = zeta(I,J,bi,bj)*eplus - eta(I,J,bi,bj)*eminus
     &         - 0.5 _d 0 * PRESS(I,J,bi,bj)
         ENDDO
        ENDDO

        DO j=1,sNy+1
         DO i=1,sNx+1
          sig12(I,J) = 2. _d 0 * e12(I,J,bi,bj) * etaZ(I,J,bi,bj)
         ENDDO
        ENDDO
C
C     compute divergence of stress tensor
C
        DO J=1,sNy
         DO I=1,sNx
          stressDivergenceX(I,J,bi,bj) =
     &         ( sig11(I  ,J  ) * _dyF(I  ,J,bi,bj)
     &         - sig11(I-1,J  ) * _dyF(I-1,J,bi,bj)
     &         + sig12(I  ,J+1) * _dxV(I,J+1,bi,bj)
     &         - sig12(I  ,J  ) * _dxV(I,J  ,bi,bj)
     &         ) * recip_rAw(I,J,bi,bj)
          stressDivergenceY(I,J,bi,bj) =
     &         ( sig22(I  ,J  ) * _dxF(I,J  ,bi,bj)
     &         - sig22(I  ,J-1) * _dxF(I,J-1,bi,bj)
     &         + sig12(I+1,J  ) * _dyU(I+1,J,bi,bj)
     &         - sig12(I  ,J  ) * _dyU(I  ,J,bi,bj)
     &         ) * recip_rAs(I,J,bi,bj)
         ENDDO
        ENDDO
C     compute lhs side of momentum equations
        DO J=1,sNy
         DO I=1,sNx
C     mass*(uIce)/deltaT - dsigma/dx
          uIceLHS(I,J,bi,bj) = 
     &         bdfAlpha*seaiceMassU(I,J,bi,bj)*recip_deltaT
     &         *uIceLoc(I,J,bi,bj) - stressDivergenceX(I,J,bi,bj)
C     mass*(vIce)/deltaT - dsigma/dy
          vIceLHS(I,J,bi,bj) = 
     &         bdfAlpha*seaiceMassV(I,J,bi,bj)*recip_deltaT
     &         *vIceLoc(I,J,bi,bj) - stressDivergenceY(I,J,bi,bj)
C     coriols terms: - mass*f*vIce
          uIceLHS(I,J,bi,bj) = uIceLHS(I,J,bi,bj) - 0.5 _d 0*(
     &         seaiceMassC(I  ,J,bi,bj) * _fCori(I  ,J,bi,bj)
     &       * 0.5 _d 0*( vIceLoc(I  ,J,bi,bj)+vIceLoc(I  ,J+1,bi,bj) )
     &       + seaiceMassC(I-1,J,bi,bj) * _fCori(I-1,J,bi,bj)
     &       * 0.5 _d 0*( vIceLoc(I-1,J,bi,bj)+vIceLoc(I-1,J+1,bi,bj) )
     &           )
C                    + mass*f*uIce
          vIceLHS(I,J,bi,bj) = vIceLHS(I,J,bi,bj) + 0.5 _d 0*(
     &         seaiceMassC(I,J  ,bi,bj) * _fCori(I,J  ,bi,bj)
     &       * 0.5 _d 0*( uIceLoc(I,J  ,bi,bj)+uIceLoc(I+1,  J,bi,bj) )
     &       + seaiceMassC(I,J-1,bi,bj) * _fCori(I,J-1,bi,bj)
     &       * 0.5 _d 0*( uIceLoc(I,J-1,bi,bj)+uIceLoc(I+1,J-1,bi,bj) )
     &           )
C     ocean-ice drag terms: + Cdrag*(uIce*cosWat - vIce*sinWat)
          uIceLHS(I,J,bi,bj) = uIceLHS(I,J,bi,bj) +
     &         0.5 _d 0 * ( DWATN(I,J,bi,bj)+DWATN(I-1,J,bi,bj) ) *
     &         COSWAT * uIceLoc(I,J,bi,bj)
     &         - SIGN(SINWAT, _fCori(I,J,bi,bj))* 0.5 _d 0 *
     &         (  DWATN(I  ,J,bi,bj) * 0.5 _d 0 *
     &         (vIceLoc(I  ,J,bi,bj)+vIceLoc(I  ,J+1,bi,bj))
     &         +  DWATN(I-1,J,bi,bj) * 0.5 _d 0 *
     &         (vIceLoc(I-1,J,bi,bj)+vIceLoc(I-1,J+1,bi,bj))
     &         )
C                           + Cdrag*(vIce*cosWat + uIce*sinWat)
          vIceLHS(I,J,bi,bj) = vIceLHS(I,J,bi,bj) +
     &         0.5 _d 0 * ( DWATN(I,J,bi,bj)+DWATN(I,J-1,bi,bj) ) *
     &         COSWAT * vIceLoc(I,J,bi,bj)
     &         + SIGN(SINWAT, _fCori(I,J,bi,bj)) * 0.5 _d 0 *
     &         (  DWATN(I,J  ,bi,bj) * 0.5 _d 0 *
     &         (uIceLoc(I,J  ,bi,bj)+uIceLoc(I+1,J  ,bi,bj))
     &         +  DWATN(I,J-1,bi,bj) * 0.5 _d 0 *
     &         (uIceLoc(I,J-1,bi,bj)+uIceLoc(I+1,J-1,bi,bj))
     &         )
C     apply masks for interior (important when we have open boundaries)
          uIceLHS(I,J,bi,bj) = uIceLHS(I,J,bi,bj)*maskinW(I,J,bi,bj)
          vIceLHS(I,J,bi,bj) = vIceLHS(I,J,bi,bj)*maskinS(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_JFNK */

      RETURN
      END
