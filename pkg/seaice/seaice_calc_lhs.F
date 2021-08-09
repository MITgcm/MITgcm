#include "SEAICE_OPTIONS.h"

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
      _RL     COSWAT, recip_deltaT
C     backward difference extrapolation factor
      _RL bdfAlpha
C     symmetric drag coefficient
      _RL dragSym(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     fractional area at velocity points
      _RL areaW(1:sNx,1:sNy)
      _RL areaS(1:sNx,1:sNy)
#ifdef SEAICE_ALLOW_MOM_ADVECTION
C     tendency due to advection of momentum
      _RL gUmom(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gVmom(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif /*  SEAICE_ALLOW_MOM_ADVECTION */
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

C     initialise fractional areas at velocity points
      DO J=1,sNy
       DO I=1,sNx
        areaW(I,J) = 1. _d 0
        areaS(I,J) = 1. _d 0
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     symmetric drag coefficient may include bottomdrag for grounded ice
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          dragSym(I,J) = DWATN(I,J,bi,bj)*COSWAT
#ifdef SEAICE_ALLOW_BOTTOMDRAG
     &         +CbotC(I,J,bi,bj)
#endif /* SEAICE_ALLOW_BOTTOMDRAG */
         ENDDO
        ENDDO
C
C     compute components of stress tensor from current velocity field
C     and compute divergence of stress tensor
C
        CALL SEAICE_CALC_STRESSDIV(
     I       e11, e22, e12, press, zeta, eta, etaZ,
     O       stressDivergenceX, stressDivergenceY,
     I       bi, bj, myTime, myIter, myThid )
C     compute lhs side of momentum equations
        IF ( SEAICEscaleSurfStress ) THEN
         DO J=1,sNy
          DO I=1,sNx
           areaW(I,J) = 0.5 _d 0*(AREA(I,J,bi,bj)+AREA(I-1,J,bi,bj))
           areaS(I,J) = 0.5 _d 0*(AREA(I,J,bi,bj)+AREA(I,J-1,bi,bj))
          ENDDO
         ENDDO
        ENDIF
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
C     ocean-ice and bottom drag terms: + (Cdrag*cosWat+Cb)*uIce - vIce*sinWat)
          uIceLHS(I,J,bi,bj) = uIceLHS(I,J,bi,bj) + (
     &         0.5 _d 0 * ( dragSym(I,J)+dragSym(I-1,J) )
     &         * uIceLoc(I,J,bi,bj)
     &         - SIGN(SINWAT, _fCori(I,J,bi,bj))* 0.5 _d 0 *
     &         (  DWATN(I  ,J,bi,bj) * 0.5 _d 0 *
     &         (vIceLoc(I  ,J,bi,bj)+vIceLoc(I  ,J+1,bi,bj))
     &         +  DWATN(I-1,J,bi,bj) * 0.5 _d 0 *
     &         (vIceLoc(I-1,J,bi,bj)+vIceLoc(I-1,J+1,bi,bj))
     &         ) ) * areaW(I,J)
C                                      + (Cdrag*cosWat+Cb)*uIce + uIce*sinWat)
          vIceLHS(I,J,bi,bj) = vIceLHS(I,J,bi,bj) + (
     &         0.5 _d 0 * ( dragSym(I,J)+dragSym(I,J-1) )
     &         * vIceLoc(I,J,bi,bj)
     &         + SIGN(SINWAT, _fCori(I,J,bi,bj)) * 0.5 _d 0 *
     &         (  DWATN(I,J  ,bi,bj) * 0.5 _d 0 *
     &         (uIceLoc(I,J  ,bi,bj)+uIceLoc(I+1,J  ,bi,bj))
     &         +  DWATN(I,J-1,bi,bj) * 0.5 _d 0 *
     &         (uIceLoc(I,J-1,bi,bj)+uIceLoc(I+1,J-1,bi,bj))
     &         ) ) * areaS(I,J)
C     apply masks for interior (important when we have open boundaries)
          uIceLHS(I,J,bi,bj) = uIceLHS(I,J,bi,bj)*maskinW(I,J,bi,bj)
          vIceLHS(I,J,bi,bj) = vIceLHS(I,J,bi,bj)*maskinS(I,J,bi,bj)
         ENDDO
        ENDDO
#ifdef SEAICE_ALLOW_MOM_ADVECTION
        IF ( SEAICEmomAdvection ) THEN
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
           gUmom(I,J) = 0. _d 0
           gVmom(I,J) = 0. _d 0
          ENDDO
         ENDDO
         CALL SEAICE_MOM_ADVECTION(
     I        bi,bj,1,sNx,1,sNy,
     I        uIceLoc, vIceLoc,
     O        gUmom, gVmom,
     I        myTime, myIter, myThid )
C     Beware of sign! gU/Vmom is computed for the rhs of the equation;
C     therefore, we need to substract gU/Vmom from the left hand side
         DO J=1,sNy
          DO I=1,sNx
           uIceLHS(I,J,bi,bj) = uIceLHS(I,J,bi,bj) - gUmom(I,J)
           vIceLHS(I,J,bi,bj) = vIceLHS(I,J,bi,bj) - gVmom(I,J)
          ENDDO
         ENDDO
        ENDIF
#endif /* SEAICE_ALLOW_MOM_ADVECTION */
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_JFNK */

      RETURN
      END
