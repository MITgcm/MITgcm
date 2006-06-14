C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_ocean_stress.F,v 1.9 2006/06/14 16:08:36 mlosch Exp $
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
#include "SEAICE_FFIELDS.h"

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
      _RL  fuIce, fvIce, FX, FY
      _RL  areaW, areaS

      _RL press       (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL etaPlusZeta (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL zetaMinusEta(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL etaMeanZ    (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL etaMeanU    (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL etaMeanV    (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dVdx        (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dVdy        (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dUdx        (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dUdy        (1-Olx:sNx+Olx,1-Oly:sNy+Oly)

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
C     recompute viscosities from updated ice velocities
       CALL SEAICE_CALC_VISCOSITIES( 
     I      uIce(1-Olx,1-Oly,1,1,1), vIce(1-Olx,1-Oly,1,1,1), 
     I      zMin, zMax, hEffM, press0,
     O      eta, zeta, press, 
#ifdef SEAICE_ALLOW_EVP
     O      seaice_div, seaice_tension, seaice_shear,
#endif /* SEAICE_ALLOW_EVP */
     I      myThid )
C     re-compute internal stresses with updated ice velocities
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-Oly+1,sNy+Oly-1
          DO i=1-Olx+1,sNx+Olx-1
           etaPlusZeta (I,J) =  eta(I,J,bi,bj) + zeta(I,J,bi,bj)
           zetaMinusEta(I,J) = zeta(I,J,bi,bj) -  eta(I,J,bi,bj)
           etaMeanU (I,J) =
     &          HALF*(ETA (I,J,bi,bj) + ETA (I-1,J  ,bi,bj))
           etaMeanV (I,J) =
     &          HALF*(ETA (I,J,bi,bj) + ETA (I  ,J-1,bi,bj))
           etaMeanZ (I,J) = QUART *  
     &          ( eta(I  ,J,bi,bj) + eta(I  ,J-1,bi,bj) 
     &          + eta(I-1,J,bi,bj) + eta(I-1,J-1,bi,bj) )
           dUdx(I,J) = ( uIce(I+1,J,1,bi,bj) - uIce(I,J,1,bi,bj) )
     &          * _recip_dxF(I,J,bi,bj)
           dUdy(I,J) = ( uIce(I,J+1,1,bi,bj) - uIce(I,J,1,bi,bj) )
     &          * _recip_dyU(I,J+1,bi,bj)
           dVdx(I,J) = ( vIce(I+1,J,1,bi,bj) - vIce(I,J,1,bi,bj) )
     &          * _recip_dxV(I+1,J,bi,bj)
           dVdy(I,J) = ( vIce(I,J+1,1,bi,bj) - vIce(I,J,1,bi,bj) )
     &          * _recip_dyF(I,J,bi,bj)
          ENDDO
         ENDDO
         DO J = 1,sNy
          DO I = 1,sNx
C     First FX = (d/dx)*sigma
C     + d/dx[ eta+zeta d/dx ] U
           FX = _recip_dxC(I,J,bi,bj) *
     &            ( etaPlusZeta(I  ,J) * dUdx(I  ,J) 
     &            - etaPlusZeta(I-1,J) * dUdx(I-1,J) )
C     + (d/dy)[eta*(d/dy + tanphi/a)] U (also on UVRT1/2)
           FX = FX + _recip_dyG(I,J,bi,bj) * (
     &          ( etaMeanZ(I,J+1) * dUdy(I,J+1)
     &          - etaMeanZ(I,J  ) * dUdy(I,J  )
     &          )
     &          - ( etaMeanZ(I,J+1) 
     &            * ( uIce(I,J+1,1,bi,bj)+uIce(I,J,1,bi,bj) )
     &            - etaMeanZ(I,J  )
     &            * ( uIce(I,J-1,1,bi,bj)+uIce(I,J,1,bi,bj) ) )
     &          * 0.5 _d 0 * _tanPhiAtU(I,J,bi,bj)
     &          * recip_rSphere )
C     - 2*eta*(tanphi/a) * ( tanphi/a ) U
           FX = FX - TWO * uIce(I,J,1,bi,bj)
     &          * etaMeanU(I,J)*recip_rSphere*recip_rSphere
     &          * _tanPhiAtU(I,J,bi,bj)  * _tanPhiAtU(I,J,bi,bj)
C     + d/dx[ (zeta-eta) dV/dy]
           FX = FX +
     &          ( zetaMinusEta(I  ,J  ) * dVdy(I  ,J  )
     &          - zetaMinusEta(I-1,J  ) * dVdy(I-1,J  )
     &          ) * _recip_dxC(I,J,bi,bj) 
C     + d/dy[ eta dV/x ]
           FX = FX + (
     &            etaMeanZ(I,J+1)
     &          * ( vIce(I  ,J+1,1,bi,bj) - vIce(I-1,J+1,1,bi,bj) )
     &          * _recip_dxV(I,J+1,bi,bj)
     &          - etaMeanZ(I,J  )
     &          * ( vIce(I  ,J,1,bi,bj) - vIce(I-1,J,1,bi,bj) )
     &          * _recip_dxV(I,J,bi,bj)
     &          ) * _recip_dyG(I,J,bi,bj)
C     - d/dx[ (eta+zeta) * v * (tanphi/a) ]
           FX = FX - (
     &            etaPlusZeta(I  ,J) 
     &          * 0.5 * (vIce(I  ,J,1,bi,bj)+vIce(I  ,J+1,1,bi,bj))
     &          * 0.5 * ( _tanPhiAtU(I  ,J,bi,bj) 
     &          + _tanPhiAtU(I+1,J,bi,bj) )
     &          - etaPlusZeta(I-1,J) *
     &          * 0.5 * (vIce(I-1,J,1,bi,bj)+vIce(I-1,J+1,1,bi,bj))
     &          * 0.5 * ( _tanPhiAtU(I-1,J,bi,bj) 
     &          + _tanPhiAtU(I  ,J,bi,bj) )
     &          )* _recip_dxC(I,J,bi,bj)*recip_rSphere
C     - 2*eta*(tanphi/a) * dV/dx 
           FX = FX 
     &          -TWO * etaMeanU(I,J) * _tanPhiAtV(I,J,bi,bj)
     &          *recip_rSphere
     &          *(vIce(I  ,J,1,bi,bj) + vIce(I  ,J+1,1,bi,bj)
     &          - vIce(I-1,J,1,bi,bj) - vIce(I-1,J+1,1,bi,bj))
     &          * _recip_dxC(I,J,bi,bj)
C     - (d/dx) P/2
           FX = _maskW(I,J,1,bi,bj) * ( FX - _recip_dxC(I,J,bi,bj) 
     &          * ( press(I,J,bi,bj) - press(I-1,J,bi,bj) ) )
C
C     then FY = (d/dy)*sigma
C     + d/dy [(eta+zeta) d/dy] V
           FY = _recip_dyC(I,J,bi,bj) *
     &          ( dVdy(I,J  ) * etaPlusZeta(I,J  )
     &          - dVdy(I,J-1) * etaPlusZeta(I,J-1) )
C     + d/dx [eta d/dx] V
           FY = FY +  _recip_dxC(I,J,bi,bj) *
     &          ( eta(I  ,J,bi,bj) * dVdx(I  ,J)
     &          - eta(I-1,J,bi,bj) * dVdx(I-1,J) )
C     - d/dy [(zeta-eta) tanphi/a] V
           FY = FY - _recip_dyC(I,J,bi,bj) * recip_rSphere * (
     &            zetaMinusEta(I,J  ) * tanPhiAtU(I,J  ,bi,bj)
     &          * 0.5 * ( vIce(I,J,1,bi,bj) + vIce(I,J+1,1,bi,bj))
     &          - zetaMinusEta(I,J-1) * tanPhiAtU(I,J-1,bi,bj)
     &          * 0.5 * ( vIce(I,J,1,bi,bj) + vIce(I,J-1,1,bi,bj)) )
C     2*eta tanphi/a ( - tanphi/a - d/dy) V
           FY = FY - TWO*etaMeanV(I,J) * recip_rSphere
     &          * _tanPhiAtV(I,J,bi,bj) * (
     &            _tanPhiAtV(I,J,bi,bj) * recip_rSphere
     &          + _recip_dyC(I,J,bi,bj) *
     &          ( 0.5 * ( vIce(I,J,1,bi,bj) + vIce(I,J+1,1,bi,bj))
     &          - 0.5 * ( vIce(I,J,1,bi,bj) + vIce(I,J-1,1,bi,bj)) ) )
C     + d/dy[ (zeta-eta) dU/dx ]
           FY = FY +
     &          ( zetaMinusEta(I,J  )*dUdx(I,J  )
     &          - zetaMinusEta(I,J-1)*dUdx(I,J-1) )
     &          * _recip_dyC(I,J,bi,bj)
C     + d/dx[ eta dU/dy ]
           FY = FY + _recip_dxG(I,J,bi,bj) *
     &          ( etaMeanZ(I+1,J) * dUdy(I+1,J)
     &          - etaMeanZ(I  ,J) * dUdy(I  ,J) )
C     + d/dx[ eta * (tanphi/a) * U ]
           FY = FY + (
     &            etaMeanZ(I+1,J) * 0.5 * 
     &          ( uIce(I+1,J  ,1,bi,bj) * _tanPhiAtU(I+1,J  ,bi,bj)
     &          + uIce(I+1,J-1,1,bi,bj) * _tanPhiAtU(I+1,J-1,bi,bj) ) 
     &          - etaMeanZ(I  ,J) * 0.5 * 
     &          ( uIce(I  ,J  ,1,bi,bj) * _tanPhiAtU(I  ,J  ,bi,bj) 
     &          + uIce(I  ,J-1,1,bi,bj) * _tanPhiAtU(I  ,J  ,bi,bj) ) 
     &          ) *  _recip_dxG(I,J,bi,bj)*recip_rSphere
C     + 2*eta*(tanphi/a) dU/dx
           FY = FY +
     &          TWO * etaMeanV(I,J)*TWO  * _tanPhiAtV(I,J,bi,bj)
     &          * ( uIce(I+1,J,1,bi,bj)+uIce(I+1,J-1,1,bi,bj)
     &            - uIce(I  ,J,1,bi,bj)-uIce(I  ,J-1,1,bi,bj) )
     &          * _recip_dxG(I,J,bi,bj) * recip_rSphere 
C     - (d/dy) P/2
           FY = _maskS(I,J,1,bi,bj) * ( FY - _recip_dyC(I,J,bi,bj) 
     &          * ( press(I,J,bi,bj) - press(I,J-1,bi,bj) ) )
C     
C     recompute wind stress over ice (done already in seaice_dynsolver, 
C     but not saved)
           fuIce = 0.5 _d 0 * 
     &          ( DAIRN(I  ,J,bi,bj)*(
     &          COSWIN*uWind(I  ,J,bi,bj)
     &          -SIGN(SINWIN, _fCori(I  ,J,bi,bj))*vWind(I  ,J,bi,bj) )
     &          + DAIRN(I-1,J,bi,bj)*(
     &          COSWIN*uWind(I-1,J,bi,bj)
     &          -SIGN(SINWIN, _fCori(I-1,J,bi,bj))*vWind(I-1,J,bi,bj) )
     &          )
           fvIce = 0.5 _d 0 * 
     &          ( DAIRN(I,J  ,bi,bj)*(
     &          SIGN(SINWIN, _fCori(I  ,J,bi,bj))*uWind(I,J  ,bi,bj)
     &          +COSWIN*vWind(I,J  ,bi,bj) )
     &          + DAIRN(I,J-1,bi,bj)*(
     &          SIGN(SINWIN, _fCori(I,J-1,bi,bj))*uWind(I,J-1,bi,bj)
     &          +COSWIN*vWind(I,J-1,bi,bj) )
     &          )
C     average wind stress over ice and ocean and apply averaged wind 
C     stress and internal ice stresses to surface layer of ocean
           areaW = 0.5 * (AREA(I,J,1,bi,bj) + AREA(I-1,J,1,bi,bj))
     &         * SEAICEstressFactor
           areaS = 0.5 * (AREA(I,J,1,bi,bj) + AREA(I,J-1,1,bi,bj))
     &         * SEAICEstressFactor
           fu(I,J,bi,bj)=(ONE-areaW)*fu(I,J,bi,bj)+areaW*fuIce
     &          + FX * SEAICEstressFactor
           fv(I,J,bi,bj)=(ONE-areaS)*fv(I,J,bi,bj)+areaS*fvIce
     &          + FY * SEAICEstressFactor
          END DO
         END DO
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
          fuIce=HALF*( DWATN(I,J,bi,bj)+DWATN(I,J+1,bi,bj) )*
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
          fvIce=HALF*( DWATN(I,J,bi,bj)+DWATN(I+1,J,bi,bj) )*
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
          fu(I,J,bi,bj)=(ONE-areaW)*fu(I,J,bi,bj)+areaW*fuIce
          fv(I,J,bi,bj)=(ONE-areaS)*fv(I,J,bi,bj)+areaS*fvIce
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      ENDIF
      CALL EXCH_UV_XY_RS(fu, fv, .TRUE., myThid)

#endif /* not SEAICE_CGRID */

      RETURN
      END
