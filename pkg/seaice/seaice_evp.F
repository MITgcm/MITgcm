C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_evp.F,v 1.5 2007/04/22 19:56:22 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_EVP( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_EVP                                    |
C     | o Ice dynamics using an EVP solver following             |
C     |   E. C. Hunke and J. K. Dukowicz. An                     |
C     |   Elastic-Viscous-Plastic Model for Sea Ice Dynamics,    |
C     |   J. Phys. Oceanogr., 27, 1849-1867 (1997).              |
C     |==========================================================|
C     | written by Martin Losch, March 2006                      |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE.h"
#include "SEAICE_PARAMS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#if ( defined (SEAICE_CGRID) && \
      defined (SEAICE_ALLOW_EVP) && \
      defined (SEAICE_ALLOW_DYNAMICS) )

C     === Local variables ===
C     i,j,bi,bj     - Loop counters
C     nEVPstep      - number of timesteps within the EVP solver
C     iEVPstep      - Loop counter
C     SIN/COSWAT    - sine/cosine of turning angle
C     recip_evp_tau - inverse of EVP relaxation/damping timescale
C     ecc2          - eccentricity squared
C     e11,e12,e22   - components of strain rate tensor
C     PRESS         - (modified) pressure (includes upper/lower bounds etc.)
C     stressDivergenceX/Y - divergence of stress tensor
C     sig11, sig22  - sum and difference of diagonal terms of stress tensor
      
      INTEGER i, j, bi, bj
      INTEGER nEVPstep, iEVPstep
      INTEGER ikeyloc
#ifndef ALLOW_AUTODIFF_TAMC
      integer nEVPstepMax
#endif

      _RL SINWAT, COSWAT
      _RL TEMPVAR, ecc2, recip_ecc2, recip_evp_tau

      _RL e11              (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e22              (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e12              (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL PRESS            (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL stressDivergenceX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL stressDivergenceY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig11            (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig22            (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     auxilliary variables
      _RL e11C, e22C, e12C, deltaC
      _RL e11Z, e22Z, e12Z, deltaZ
      _RL zetaLoc

C--   introduce turning angles
      SINWAT=SIN(SEAICE_waterTurnAngle*deg2rad)
      COSWAT=COS(SEAICE_waterTurnAngle*deg2rad)

C     abbreviation eccentricity squared
      ecc2=SEAICE_eccen**2
      recip_ecc2 = 0. _d 0
      IF ( ecc2 .NE. 0. _d 0 ) recip_ecc2=ONE/ecc2
C     determine number of interal time steps
      nEVPstep = INT(SEAICE_deltaTdyn/SEAICE_deltaTevp)
C     inverse relaxation/damping time scale
      recip_evp_tau = 0. _d 0
      IF ( SEAICE_elasticParm .GT. 0. )
     &     recip_evp_tau = 1./(SEAICE_deltaTdyn*SEAICE_elasticParm)
#ifndef ALLOW_AUTODIFF_TAMC
      nEVPstepMax = nEVPstep
#endif

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
C     save last external time step (useless, but consistent with 
C     VP-solver)
          uIce (I,J,2,bi,bj) = uIce(I,J,1,bi,bj)
          vIce (I,J,2,bi,bj) = vIce(I,J,1,bi,bj)
C     use u/vIceC as work arrays: copy previous time step to u/vIceC
          uIceC(I,J,bi,bj)   = uIce(I,J,1,bi,bj)
          vIceC(I,J,bi,bj)   = vIce(I,J,1,bi,bj)
c     initialise strain rates
          e11  (I,J,bi,bj)   = 0. _d 0
          e22  (I,J,bi,bj)   = 0. _d 0
          e12  (I,J,bi,bj)   = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
C     start of the main time loop
      DO iEVPstep = 1, nEVPstepMax
       IF (iEVPstep.LE.nEVPstep) THEN
C
#ifdef ALLOW_AUTODIFF_TAMC
        ikeyloc = iEVPstep + 
     &      (ikey_dynamics-1)*nEVPstepMax
CADJ STORE uicec  = comlev1_evp,
CADJ &     key = ikeyloc, byte = isbyte
CADJ STORE vicec  = comlev1_evp,
CADJ &     key = ikeyloc, byte = isbyte
CADJ STORE seaice_sigma1  = comlev1_evp,
CADJ &     key = ikeyloc, byte = isbyte
CADJ STORE seaice_sigma2  = comlev1_evp,
CADJ &     key = ikeyloc, byte = isbyte
CADJ STORE seaice_sigma12 = comlev1_evp,
CADJ &     key = ikeyloc, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C
C     first calculate strain rates and bulk moduli/viscosities
C     
        CALL SEAICE_CALC_STRAINRATES( 
     I       uIceC, vIceC,
     O       e11, e22, e12,
     I       myThid )

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
C     average strain rates to Z and C points
            e11C = e11(i,j,bi,bj)
            e22C = e22(i,j,bi,bj)
            e12C = 0.25 * ( e12(I,J  ,bi,bj) + e12(I+1,J  ,bi,bj) 
     &           +          e12(I,J+1,bi,bj) + e12(I+1,J+1,bi,bj) )
            e11Z = 0.25 * ( e11(I,J  ,bi,bj) + e11(I-1,J  ,bi,bj) 
     &           +          e11(I,J-1,bi,bj) + e11(I-1,J-1,bi,bj) )
            e22Z = 0.25 * ( e22(I,J  ,bi,bj) + e22(I-1,J  ,bi,bj) 
     &           +          e22(I,J-1,bi,bj) + e22(I-1,J-1,bi,bj) )
            e12Z = e12(i,j,bi,bj)
            deltaC = (e11C**2+e22C**2)*(ONE+recip_ecc2)
     &           + 4. _d 0*recip_ecc2*e12C**2 
     &           + 2. _d 0*e11C*e22C*(ONE-recip_ecc2)
            deltaC = SQRT(MAX(deltaC,SEAICE_EPS_SQ))
            deltaZ = (e11Z**2+e22Z**2)*(ONE+recip_ecc2)
     &           + 4. _d 0*recip_ecc2*e12Z**2 
     &           + 2. _d 0*e11Z*e22Z*(ONE-recip_ecc2)
            deltaZ = SQRT(MAX(deltaZ,SEAICE_EPS_SQ))
C     modify pressure
            zetaLoc          = HALF*press0(I,J,bi,bj)/deltaC
            zetaLoc          = MIN(zMax(I,J,bi,bj),zetaLoc)
            zetaLoc          = MAX(zMin(I,J,bi,bj),zetaLoc)
            zetaLoc          = zetaLoc*hEffM(I,J,bi,bj) 
            press(I,J,bi,bj) = TWO*zetaLoc*deltaC
CMLC     not clear if it is better to use the original pressure
CML            press(I,J,bi,bj) = press0(i,j,bi,bj)
C     
            seaice_div    (I,J,bi,bj) = (e11C+e22C)/deltaC
     &           *hEffM(I,J,bi,bj)
            seaice_tension(I,J,bi,bj) = (e11C-e22C)/deltaC
     &           *hEffM(I,J,bi,bj)
            seaice_shear  (I,J,bi,bj) = 2. _d 0*e12Z/deltaZ
CML     &         *hEffM(I  ,J  ,bi,bj)*hEffM(I-1,J  ,bi,bj)
CML     &         *hEffM(I  ,J-1,bi,bj)*hEffM(I-1,J-1,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C--   Update overlap regions
        _EXCH_XY_R8(press         , myThid)
        _EXCH_XY_R8(seaice_div    , myThid)
        _EXCH_XY_R8(seaice_tension, myThid)
        CALL EXCH_Z_3D_RL(seaice_shear, 1, myThid)
C
C     set up rhs for stepping the velocity field
C
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-Oly+1,sNy+Oly-1
           DO i=1-Olx+1,sNx+Olx-1
C     set up non-linear water drag, forcex, forcey
            TEMPVAR = QUART*(
     &           ( uIceC(I  ,J,bi,bj)-GWATX(I  ,J,bi,bj)
     &           + uIceC(I+1,J,bi,bj)-GWATX(I+1,J,bi,bj))**2
     &           +(vIceC(I,J  ,bi,bj)-GWATY(I,J  ,bi,bj)
     &           + vIceC(I,J+1,bi,bj)-GWATY(I,J+1,bi,bj))**2)
            IF ( TEMPVAR .LE. (QUART/SEAICE_waterDrag)**2 ) THEN
             DWATN(I,J,bi,bj)=QUART
            ELSE
             DWATN(I,J,bi,bj)=SEAICE_waterDrag*SQRT(TEMPVAR)
            ENDIF
            DWATN(I,J,bi,bj) = DWATN(I,J,bi,bj) * HEFFM(I,J,bi,bj)
C     set up symmetric drag
            DRAGS(I,J,bi,bj) = DWATN(I,J,bi,bj)*COSWAT
C     set up anti symmetric drag force and add in ice ocean stress
C     ( remember to average to correct velocity points )
            FORCEX(I,J,bi,bj)=FORCEX0(I,J,bi,bj)+
     &           0.5*( DWATN(I,J,bi,bj)+DWATN(I-1,J,bi,bj) ) *
     &           COSWAT * GWATX(I,J,bi,bj) 
     &           - SIGN(SINWAT, _fCori(I,J,bi,bj))* 0.5 _d 0 * 
     &           ( DWATN(I  ,J,bi,bj) *
     &           0.5 _d 0 * (GWATY(I  ,J  ,bi,bj)-vIceC(I  ,J  ,bi,bj)
     &                      +GWATY(I  ,J+1,bi,bj)-vIceC(I  ,J+1,bi,bj))
     &           + DWATN(I-1,J,bi,bj) *
     &           0.5 _d 0 * (GWATY(I-1,J  ,bi,bj)-vIceC(I-1,J  ,bi,bj)
     &                      +GWATY(I-1,J+1,bi,bj)-vIceC(I-1,J+1,bi,bj))
     &         )
            FORCEY(I,J,bi,bj)=FORCEY0(I,J,bi,bj)+
     &           0.5*( DWATN(I,J,bi,bj)+DWATN(I,J-1,bi,bj) ) *
     &           COSWAT * GWATY(I,J,bi,bj) 
     &           + SIGN(SINWAT, _fCori(I,J,bi,bj)) * 0.5 _d 0 * 
     &           ( DWATN(I,J  ,bi,bj) *
     &           0.5 _d 0 * (GWATX(I  ,J  ,bi,bj)-uIceC(I  ,J  ,bi,bj)
     &                      +GWATX(I+1,J  ,bi,bj)-uIceC(I+1,J  ,bi,bj))
     &           + DWATN(I,J-1,bi,bj) *
     &           0.5 _d 0 * (GWATX(I  ,J-1,bi,bj)-uIceC(I  ,J-1,bi,bj)
     &                      +GWATX(I+1,J-1,bi,bj)-uIceC(I+1,J-1,bi,bj))
     &         )
C     coriols terms
            FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj) + 0.5*( 
     &             seaiceMassC(I  ,J,bi,bj) * _fCori(I  ,J,bi,bj)
     &           * 0.5*( vIceC(I  ,J,bi,bj)+vIceC(I  ,J+1,bi,bj) )
     &           + seaiceMassC(I-1,J,bi,bj) * _fCori(I-1,J,bi,bj)
     &           * 0.5*( vIceC(I-1,J,bi,bj)+vIceC(I-1,J+1,bi,bj) ) )
            FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj) - 0.5*( 
     &             seaiceMassC(I,J  ,bi,bj) * _fCori(I,J  ,bi,bj)
     &           * 0.5*( uIceC(I  ,J  ,bi,bj)+uIceC(I+1,  J,bi,bj) )
     &           + seaiceMassC(I,J-1,bi,bj) * _fCori(I,J-1,bi,bj)
     &           * 0.5*( uIceC(I  ,J-1,bi,bj)+uIceC(I+1,j-1,bi,bj) ) )
         ENDDO
        ENDDO

C     first step stress equations
          DO j=1-Oly+1,sNy+Oly
           DO i=1-Olx+1,sNx+Olx
C     sigma1 and sigma2 are computed on C points
            seaice_sigma1 (I,J,bi,bj) = ( seaice_sigma1 (I,J,bi,bj)
     &           + SEAICE_deltaTevp *  0.5 * recip_evp_tau 
     &           * PRESS(I,J,bi,bj) * ( seaice_div(I,J,bi,bj)-1. _d 0 )
     &           )/(1. + 0.5*SEAICE_deltaTevp*recip_evp_tau)
     &           *hEffM(I,J,bi,bj)
            seaice_sigma2 (I,J,bi,bj) = ( seaice_sigma2 (I,J,bi,bj)
     &           + SEAICE_deltaTevp * 0.5 * recip_evp_tau
     &           * PRESS(I,J,bi,bj) * seaice_tension(I,J,bi,bj)
     &           )/(1. + 0.5*SEAICE_deltaTevp*recip_evp_tau*ecc2)
     &         *hEffM(I,J,bi,bj)
C     sigma12 is computed on Z points
            seaice_sigma12(I,J,bi,bj) = ( seaice_sigma12(I,J,bi,bj)
     &           + SEAICE_deltaTevp * 0.25 * recip_evp_tau *
     &           0.25*( PRESS(I,J  ,bi,bj) + PRESS(I-1,J  ,bi,bj)
     &                + PRESS(I,J-1,bi,bj) + PRESS(I-1,J-1,bi,bj) )
     &           * seaice_shear(I,J,bi,bj)
     &           )/(1. + 0.5*SEAICE_deltaTevp*recip_evp_tau*ecc2)
     &           *hEffM(I,J  ,bi,bj)*hEffM(I-1,J  ,bi,bj)
     &           *hEffM(I,J-1,bi,bj)*hEffM(I-1,J-1,bi,bj)
C     recover sigma11 and sigma22
            sig11(I,J) = 0.5 * 
     &           ( seaice_sigma1(I,J,bi,bj)+seaice_sigma2(I,J,bi,bj) )
            sig22(I,J) = 0.5 * 
     &           ( seaice_sigma1(I,J,bi,bj)-seaice_sigma2(I,J,bi,bj) )
           ENDDO
          ENDDO   
C     compute divergence of stress tensor
          DO J=1,sNy
           DO I=1,sNx
            stressDivergenceX(I,J) = 
     &           ( sig11(I  ,J  ) * _dyF(I  ,J,bi,bj)
     &           - sig11(I-1,J  ) * _dyF(I-1,J,bi,bj)
     &           + seaice_sigma12(I,J+1,bi,bj) * _dxV(I,J+1,bi,bj) 
     &           - seaice_sigma12(I,J  ,bi,bj) * _dxV(I,J  ,bi,bj)
     &           ) * recip_rAw(I,J,bi,bj)
     &           - 
     &           ( seaice_sigma12(I,J  ,bi,bj) 
     &           + seaice_sigma12(I,J+1,bi,bj) )
     &           * _tanPhiAtU(I,J,bi,bj) * recip_rSphere
     &           + 
     &           ( sig22(I,J) + sig22(I-1,J) ) * 0.5 _d 0
     &           * _tanPhiAtU(I,J,bi,bj) * recip_rSphere
C     one metric term  missing for general curvilinear coordinates
            stressDivergenceY(I,J) = 
     &           ( sig22(I,J  ) * _dxF(I,J  ,bi,bj)
     &           - sig22(I,J-1) * _dxF(I,J-1,bi,bj) 
     &           + seaice_sigma12(I+1,J,bi,bj) * _dyU(I+1,J,bi,bj)
     &           - seaice_sigma12(I  ,J,bi,bj) * _dyU(I  ,J,bi,bj) 
     &           ) * recip_rAs(I,J,bi,bj)
     &           - 
     &           ( sig22(I,J) + sig22(I,J-1) ) * 0.5 _d 0
     &           * _tanPhiAtV(I,J,bi,bj) * recip_rSphere
C     two metric terms missing for general curvilinear coordinates
           ENDDO
          ENDDO   
C
C     step momentum equations with ice-ocean stress term treated implicitly
C
          DO J=1,sNy
           DO I=1,sNx
            uIceC(I,J,bi,bj) = seaiceMaskU(I,J,bi,bj) * 
     &           (  seaiceMassU(I,J,bi,bj)/SEAICE_deltaTevp
     &           * uIceC(I,J,bi,bj)
     &           + FORCEX(I,J,bi,bj) 
     &           + stressDivergenceX(I,J) )
     &           /( 1. _d 0 - seaiceMaskU(I,J,bi,bj) 
     &           + seaiceMassU(I,J,bi,bj)/SEAICE_deltaTevp 
     &           + 0.5 _d 0*( DRAGS(I,J,bi,bj) + DRAGS(I-1,J,bi,bj) ) )
            vIceC(I,J,bi,bj) = seaiceMaskV(I,J,bi,bj) * 
     &           ( seaiceMassV(I,J,bi,bj)/SEAICE_deltaTevp 
     &           * vIceC(I,J,bi,bj)
     &           + FORCEY(I,J,bi,bj) 
     &           + stressDivergenceY(I,J) )
     &           /( 1. _d 0 - seaiceMaskV(I,J,bi,bj) 
     &           + seaiceMassV(I,J,bi,bj)/SEAICE_deltaTevp 
     &           + 0.5 _d 0*( DRAGS(I,J,bi,bj) + DRAGS(I,J-1,bi,bj) ) )
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      
        _EXCH_XY_R8( seaice_sigma1 , myThid )
        _EXCH_XY_R8( seaice_sigma2 , myThid )
        _EXCH_XY_R8( seaice_sigma12, myThid )
        CALL EXCH_UV_XY_RL(uIceC,vIceC,.TRUE.,myThid)
        
       ENDIF
C     end of the main time loop
      ENDDO 

C     Copy work arrays and apply masks
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          uIce(I,J,1,bi,bj)=uIceC(I,J,bi,bj)* _maskW(I,J,1,bi,bj)
          vIce(I,J,1,bi,bj)=vIceC(I,J,bi,bj)* _maskS(I,J,1,bi,bj)
         END DO
        END DO
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_DYNAMICS and SEAICE_CGRID and SEAICE_ALLOW_EVP */

      RETURN
      END
