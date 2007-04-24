C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_ocean_stress.F,v 1.15 2007/04/24 17:54:44 heimbach Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_OCEAN_STRESS( 
     I     myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_OCEAN_STRESS                           |
C     | o Calculate ocean surface stresses                       |
C     |   - C-grid version                                       |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "SEAICE.h"
#include "SEAICE_PARAMS.h"

C     === Routine arguments ===
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef SEAICE_CGRID 
C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      _RL  SINWAT, COSWAT, SINWIN, COSWIN
      _RL  fuIceLoc, fvIceLoc, FX, FY
      _RL  areaW, areaS

      _RL e11         (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL e22         (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL e12         (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL press       (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL sig11       (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL sig22       (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL sig12       (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL eplus, eminus

c     introduce turning angle (default is zero)
      SINWAT=SIN(SEAICE_waterTurnAngle*deg2rad)
      COSWAT=COS(SEAICE_waterTurnAngle*deg2rad)
      SINWIN=SIN(SEAICE_airTurnAngle*deg2rad)
      COSWIN=COS(SEAICE_airTurnAngle*deg2rad)

C--   Update overlap regions
      CALL EXCH_UV_XY_RL(WINDX, WINDY, .TRUE., myThid)

#ifndef SEAICE_EXTERNAL_FLUXES
C--   Interpolate wind stress (N/m^2) from C-points of C-grid
C     to U and V points of C-grid for forcing the ocean model.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          fu(I,J,bi,bj)=0.5*(WINDX(I,J,bi,bj) + WINDX(I-1,J,bi,bj))
          fv(I,J,bi,bj)=0.5*(WINDY(I,J,bi,bj) + WINDY(I,J-1,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ifndef SEAICE_EXTERNAL_FLUXES */

      IF ( useHB87StressCoupling ) THEN
C
C     use an intergral over ice and ocean surface layer to define 
C     surface stresses on ocean following Hibler and Bryan (1987, JPO)
C     
C     recompute strain rates, viscosities, etc. from updated ice velocities
       IF ( .NOT. SEAICEuseEVP ) THEN
C     we already have the stress components and do not need to recompute them
        CALL SEAICE_CALC_STRAINRATES( 
     I       uIce(1-Olx,1-Oly,1,1,1), vIce(1-Olx,1-Oly,1,1,1),
     O       e11, e22, e12,
     I       myThid )

        CALL SEAICE_CALC_VISCOSITIES( 
     I       e11, e22, e12, zMin, zMax, hEffM, press0,
     O       eta, zeta, press, 
     I       myThid )
       ENDIF
C     re-compute internal stresses with updated ice velocities
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         IF ( .NOT. SEAICEuseEVP ) THEN
C     only for EVP we already have computed the stress divergences, for 
C     anything else we have to do it here
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            sig11(I,J) = 0. _d 0
            sig22(I,J) = 0. _d 0
            sig12(I,J) = 0. _d 0
           ENDDO
          ENDDO

          DO j=1-Oly+1,sNy+Oly-1
           DO i=1-Olx+1,sNx+Olx-1
            eplus = e11(I,J,bi,bj) + e22(I,J,bi,bj)
            eminus= e11(I,J,bi,bj) - e22(I,J,bi,bj)
            sig11(I,J) = zeta(I,J,bi,bj)*eplus + eta(I,J,bi,bj)*eminus
     &           - 0.5 _d 0 * PRESS(I,J,bi,bj)
            sig22(I,J) = zeta(I,J,bi,bj)*eplus - eta(I,J,bi,bj)*eminus
     &           - 0.5 _d 0 * PRESS(I,J,bi,bj)
            sig12(I,J) = 2. _d 0 * e12(I,J,bi,bj) *
     &           ( eta(I,J  ,bi,bj) + eta(I-1,J  ,bi,bj)
     &           + eta(I,J-1,bi,bj) + eta(I-1,J-1,bi,bj) )
     &           /MAX(1. _d 0, 
     &             hEffM(I,J  ,bi,bj) + hEffM(I-1,J  ,bi,bj)
     &           + hEffM(I,J-1,bi,bj) + hEffM(I-1,J-1,bi,bj))
           ENDDO
          ENDDO
C     evaluate divergence of stress and apply to forcing
          DO J=1,sNy
           DO I=1,sNx
            FX = ( sig11(I  ,J  ) * _dyF(I  ,J  ,bi,bj)
     &           - sig11(I-1,J  ) * _dyF(I-1,J  ,bi,bj)
     &           + sig12(I  ,J+1) * _dxV(I  ,J+1,bi,bj) 
     &           - sig12(I  ,J  ) * _dxV(I  ,J  ,bi,bj)
     &           ) * recip_rAw(I,J,bi,bj)
     &           - 
     &           ( sig12(I,J) + sig12(I,J+1) )
     &           * _tanPhiAtU(I,J,bi,bj) * recip_rSphere
     &           + 
     &           ( sig22(I,J) + sig22(I-1,J) ) * 0.5 _d 0
     &           * _tanPhiAtU(I,J,bi,bj) * recip_rSphere
C     one metric term  missing for general curvilinear coordinates
            FY = ( sig22(I  ,J  ) * _dxF(I  ,J  ,bi,bj)
     &           - sig22(I  ,J-1) * _dxF(I  ,J-1,bi,bj) 
     &           + sig12(I+1,J  ) * _dyU(I+1,J  ,bi,bj)
     &           - sig12(I  ,J  ) * _dyU(I  ,J  ,bi,bj) 
     &           ) * recip_rAs(I,J,bi,bj)
     &           - 
     &           ( sig22(I,J) + sig22(I,J-1) ) * 0.5 _d 0
     &           * _tanPhiAtV(I,J,bi,bj) * recip_rSphere
C     two metric terms missing for general curvilinear coordinates
C     average wind stress over ice and ocean and apply averaged wind 
C     stress and internal ice stresses to surface layer of ocean
            areaW = 0.5 * (AREA(I,J,1,bi,bj) + AREA(I-1,J,1,bi,bj))
     &           * SEAICEstressFactor
            areaS = 0.5 * (AREA(I,J,1,bi,bj) + AREA(I,J-1,1,bi,bj))
     &           * SEAICEstressFactor
            fu(I,J,bi,bj)=(ONE-areaW)*fu(I,J,bi,bj)
     &           + areaW*taux(I,J,bi,bj) 
     &           + FX * SEAICEstressFactor
            fv(I,J,bi,bj)=(ONE-areaS)*fv(I,J,bi,bj)
     &           + areaS*tauy(I,J,bi,bj)
     &           + FY * SEAICEstressFactor
C     save stress divergence for later
#ifdef ALLOW_EVP
            stressDivergenceX(I,J,bi,bj) = FX
            stressDivergenceY(I,J,bi,bj) = FY
#endif 
           ENDDO
          ENDDO
         ELSE
#ifdef ALLOW_EVP
          DO J=1,sNy
           DO I=1,sNx
C     average wind stress over ice and ocean and apply averaged wind 
C     stress and internal ice stresses to surface layer of ocean
            areaW = 0.5 * (AREA(I,J,1,bi,bj) + AREA(I-1,J,1,bi,bj))
     &           * SEAICEstressFactor
            areaS = 0.5 * (AREA(I,J,1,bi,bj) + AREA(I,J-1,1,bi,bj))
     &           * SEAICEstressFactor
            fu(I,J,bi,bj)=(ONE-areaW)*fu(I,J,bi,bj)
     &           + areaW*taux(I,J,bi,bj) 
     &           + stressDivergenceX(I,J,bi,bj) * SEAICEstressFactor
            fv(I,J,bi,bj)=(ONE-areaS)*fv(I,J,bi,bj)
     &           + areaS*tauy(I,J,bi,bj)
     &           + stressDivergenceY(I,J,bi,bj) * SEAICEstressFactor
           ENDDO
          ENDDO
#endif
         ENDIF
        ENDDO
       ENDDO
      ELSE

C--   Compute ice-affected wind stress (interpolate to U/V-points) 
C     by averaging wind stress and ice-ocean stress according to 
C     ice cover
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          fuIceLoc=HALF*( DWATN(I,J,bi,bj)+DWATN(I,J+1,bi,bj) )*
     &         COSWAT * 
     &         ( UICE(I,J,1,bi,bj)-GWATX(I,J,bi,bj) )
     &         - SIGN(SINWAT, _fCori(I,J,bi,bj)) * 0.5 _d 0 * 
     &         ( DWATN(I  ,J,bi,bj) *
     &         0.5 _d 0*(vIce(I  ,J  ,1,bi,bj)-GWATY(I  ,J  ,bi,bj)
     &                  +vIce(I  ,J+1,1,bi,bj)-GWATY(I  ,J+1,bi,bj)) 
     &         + DWATN(I-1,J,bi,bj) *
     &         0.5 _d 0*(vIce(I-1,J  ,1,bi,bj)-GWATY(I-1,J  ,bi,bj)
     &                  +vIce(I-1,J+1,1,bi,bj)-GWATY(I-1,J+1,bi,bj)) 
     &         )
          fvIceLoc=HALF*( DWATN(I,J,bi,bj)+DWATN(I+1,J,bi,bj) )*
     &         COSWAT *
     &         ( VICE(I,J,1,bi,bj)-GWATY(I,J,bi,bj) )
     &         + SIGN(SINWAT,  _fCori(I,J,bi,bj)) * 0.5 _d 0 *
     &         ( DWATN(I,J  ,bi,bj) *
     &         0.5 _d 0*(uIce(I  ,J  ,1,bi,bj)-GWATX(I  ,J  ,bi,bj)
     &                  +uIce(I+1,J  ,1,bi,bj)-GWATX(I+1,J  ,bi,bj))
     &         + DWATN(I,J-1,bi,bj) *
     &         0.5 _d 0*(uIce(I  ,J-1,1,bi,bj)-GWATX(I  ,J-1,bi,bj) 
     &                  +uIce(I+1,J-1,1,bi,bj)-GWATX(I+1,J-1,bi,bj)) 
     &         )
          areaW = 0.5 _d 0 * (AREA(I,J,1,bi,bj) + AREA(I-1,J,1,bi,bj))
     &         * SEAICEstressFactor
          areaS = 0.5 _d 0 * (AREA(I,J,1,bi,bj) + AREA(I,J-1,1,bi,bj))
     &         * SEAICEstressFactor
          fu(I,J,bi,bj)=(ONE-areaW)*fu(I,J,bi,bj)+areaW*fuIceLoc
          fv(I,J,bi,bj)=(ONE-areaS)*fv(I,J,bi,bj)+areaS*fvIceLoc
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      ENDIF
      CALL EXCH_UV_XY_RS(fu, fv, .TRUE., myThid)

#endif /* not SEAICE_CGRID */

      RETURN
      END
