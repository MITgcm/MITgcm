C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_evp.F,v 1.32 2009/10/23 13:12:58 mlosch Exp $
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
#include "DYNVARS.h"
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
C     kSrf          - vertical index of surface layer
C     nEVPstep      - number of timesteps within the EVP solver
C     iEVPstep      - Loop counter
C     SIN/COSWAT    - sine/cosine of turning angle
C     recip_evp_tau - inverse of EVP relaxation/damping timescale
C     ecc2          - eccentricity squared
C     e11,e12,e22   - components of strain rate tensor
C     seaice_div    - divergence strain rates at C-points times P
C                     /divided by Delta minus 1
C     seaice_tension- tension    strain rates at C-points times P
C                     /divided by Delta
C     seaice_shear  - shear      strain rates, defined at Z-points times P
C                     /divided by Delta
C     sig11, sig22  - sum and difference of diagonal terms of stress tensor

      INTEGER i, j, bi, bj
      INTEGER kSrf
      INTEGER nEVPstep, iEVPstep
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER ikeyloc
#endif
#ifndef ALLOW_AUTODIFF_TAMC
      integer nEVPstepMax
#endif

      _RL COSWAT
      _RS SINWAT
      _RL TEMPVAR, ecc2, recip_ecc2, recip_evp_tau

      _RL seaice_div    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL seaice_tension(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL seaice_shear  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig11         (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig22         (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     auxilliary variables
      _RL e11C          (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL e22C          (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL e12C          (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pressC        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL zetaC         (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL e11Z          (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL e22Z          (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL e12Z          (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL deltaZ        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL zetaZ         (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL deltaC        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL deltaCreg, deltaZreg, pressZ
#ifdef SEAICE_ALLOW_TEM
      _RL etaDenC, zetaMaxC, etaDenZ, zetaMaxZ
#endif /* SEAICE_ALLOW_TEM */
      _RL zMaxZ, zMinZ
      _RL denom1, denom2, fac, facZ

C     surrface level
      kSrf = 1
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
      IF ( SEAICE_evpTauRelax .GT. 0. _d 0 )
     &     recip_evp_tau=1. _d 0/SEAICE_evpTauRelax
      denom1 = 1. _d 0
     &     / (1. _d 0 + 0.5 _d 0 *SEAICE_deltaTevp*recip_evp_tau)
      denom2 = 1. _d 0
     &     / (1. _d 0 + 0.5 _d 0 *SEAICE_deltaTevp*recip_evp_tau*ecc2)
#ifndef ALLOW_AUTODIFF_TAMC
      nEVPstepMax = nEVPstep
#endif

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
C     use u/vIceC as work arrays: copy previous time step to u/vIceC
          uIceC(I,J,bi,bj)   = uIce(I,J,bi,bj)
          vIceC(I,J,bi,bj)   = vIce(I,J,bi,bj)
c     initialise strain rates
          e11  (I,J,bi,bj)   = 0. _d 0
          e22  (I,J,bi,bj)   = 0. _d 0
          e12  (I,J,bi,bj)   = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     damping constraint (Hunke, J.Comp.Phys.,2001)
      IF ( SEAICE_evpDampC .GT. 0. _d 0 ) THEN
       fac = HALF * SEAICE_evpDampC * SEAICE_evpTauRelax
     &      /SEAICE_deltaTevp**2
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           zMax (I,J,bi,bj)   = _rA(I,J,bi,bj) * fac
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
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
     I       iEVPstep, myTime, myIter, myThid )

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            seaice_div    (I,J) = 0. _d 0
            seaice_tension(I,J) = 0. _d 0
            seaice_shear  (I,J) = 0. _d 0
            e11C          (I,J) = 0. _d 0
            e22C          (I,J) = 0. _d 0
            e12C          (I,J) = 0. _d 0
            pressC        (I,J) = 0. _d 0
            zetaC         (I,J) = 0. _d 0
            e11Z          (I,J) = 0. _d 0
            e22Z          (I,J) = 0. _d 0
            e12Z          (I,J) = 0. _d 0
            deltaZ        (I,J) = 0. _d 0
            zetaZ         (I,J) = 0. _d 0
            deltaC        (I,J) = 0. _d 0
           ENDDO
          ENDDO
          DO j=0,sNy+1
           DO i=0,sNx+1
C     average strain rates to Z and C points
            facZ = 0.25 _d 0
CML            facZ = 1.0 _d 0
CML     &           / MAX(1.D0,maskC(I,  J,  kSrf,bi,bj)
CML     &           +          maskC(I-1,J,  kSrf,bi,bj)
CML     &           +          maskC(I,  J-1,kSrf,bi,bj)
CML     &           +          maskC(I-1,J-1,kSrf,bi,bj) )
            e11C(I,J) = e11(I,J,bi,bj)
            e22C(I,J) = e22(I,J,bi,bj)
            e12C(I,J) = 0.25 _d 0
     &         * ( e12(I,J  ,bi,bj) + e12(I+1,J  ,bi,bj)
     &           + e12(I,J+1,bi,bj) + e12(I+1,J+1,bi,bj) )
            e11Z(I,J) = facZ
     &         * ( e11(I,J  ,bi,bj) + e11(I-1,J  ,bi,bj)
     &           + e11(I,J-1,bi,bj) + e11(I-1,J-1,bi,bj) )
            e22Z(I,J) = facZ
     &         * ( e22(I,J  ,bi,bj) + e22(I-1,J  ,bi,bj)
     &           + e22(I,J-1,bi,bj) + e22(I-1,J-1,bi,bj) )
            e12Z(I,J) = e12(I,J,bi,bj)
            deltaC(I,J) = (e11C(I,J)**2+e22C(I,J)**2)*(ONE+recip_ecc2)
     &           +      4. _d 0*recip_ecc2*e12C(I,J)**2
     &           +      2. _d 0*e11C(I,J)*e22C(I,J)*(ONE-recip_ecc2)
            deltaZ(I,J) = (e11Z(I,J)**2+e22Z(I,J)**2)*(ONE+recip_ecc2)
     &           +      4. _d 0*recip_ecc2*e12Z(I,J)**2
     &           +      2. _d 0*e11Z(I,J)*e22Z(I,J)*(ONE-recip_ecc2)
#ifdef ALLOW_AUTODIFF_TAMC
C     avoid sqrt of 0
            IF ( deltaC(I,J) .GT. 0. _d 0 )
     &           deltaC(I,J) = SQRT(deltaC(I,J))
            IF ( deltaZ(I,J) .GT. 0. _d 0 )
     &           deltaZ(I,J) = SQRT(deltaZ(I,J))
#else
            deltaC(I,J) = SQRT(deltaC(I,J))
            deltaZ(I,J) = SQRT(deltaZ(I,J))
#endif /* ALLOW_AUTODIFF_TAMC */
            deltaCreg   = MAX(deltaC(I,J),SEAICE_EPS)
            deltaZreg   = MAX(deltaZ(I,J),SEAICE_EPS)
C     modify pressure (copied from seaice_calc_viscosities)
            zetaC(I,J)  = HALF*press0(I,J,bi,bj)/deltaCreg
            pressZ      = (deltaZ(I,J)/deltaZreg) * facZ
     &         * ( PRESS0(I,J  ,bi,bj) + PRESS0(I-1,J  ,bi,bj)
     &           + PRESS0(I,J-1,bi,bj) + PRESS0(I-1,J-1,bi,bj) )
            zetaZ(I,J)  = HALF/deltaZreg * pressZ
           ENDDO
          ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE zetac  = comlev1_evp,key = ikeyloc, byte = isbyte
CADJ STORE zetaz  = comlev1_evp,key = ikeyloc, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C     regularize zeta if necessary
          DO j=0,sNy+1
           DO i=0,sNx+1
            zetaC(I,J)  = MAX(zMin(I,J,bi,bj),MIN(zMax(I,J,bi,bj) 
     &           ,zetaC(I,J)))
CML            zetaC(I,J)   = zetaC(I,J)*hEffM(I,J,bi,bj)
C
C     zMin, zMax are defined at C-points, make sure that they are not
C     masked by boundaries/land points
            zMaxZ       = MAX(
     &           MAX(zMax(I,  J,bi,bj),zMax(I,  J-1,bi,bj)),
     &           MAX(zMax(I-1,J,bi,bj),zMax(I-1,J-1,bi,bj)) )
            zMinZ       = MAX(
     &           MAX(zMin(I,  J,bi,bj),zMin(I,  J-1,bi,bj)),
     &           MAX(zMin(I-1,J,bi,bj),zMin(I-1,J-1,bi,bj)) )
            zetaZ(I,J)  = MAX(zMinZ,MIN(zMaxZ,zetaZ(I,J)))
           ENDDO
          ENDDO
C     recompute pressure
          DO j=0,sNy+1
           DO i=0,sNx+1
            pressC(I,J) = TWO*zetaC(I,J)*deltaC(I,J)
           ENDDO
          ENDDO
#ifdef ALLOW_DIAGNOSTICS
          IF ( useDiagnostics ) THEN
C     save etc, zeta, and pressure for diagnostics
           DO j=1,sNy
            DO i=1,sNx
             press(I,J,bi,bj) = pressC(I,J)
             zeta (I,J,bi,bj) = zetaC(I,J)
             eta  (I,J,bi,bj) = zetaC(I,J)*recip_ecc2
            ENDDO
           ENDDO
          ENDIF
#endif /* ALLOW_DIAGNOSTICS */
C     Calculate the RHS of the stress equations. Do this now in order to
C     avoid multiple divisions by Delta
C     P * ( D_d/Delta - 1 ) = 2*zeta*D_d - P = 2*zeta*D_d - 2*zeta*Delta
C     P * ( D_t/Delta     ) = 2*zeta*D_t
C     P * ( D_s/Delta     ) = 2*zeta*D_s
#ifdef SEAICE_ALLOW_TEM
          IF ( SEAICEuseTEM ) THEN
           DO j=0,sNy+1
            DO i=0,sNx+1
             etaDenC  = (e11C(I,J)-e22C(I,J))**2 + 4. _d 0*e12C(I,J)**2
             etaDenC  = SQRT(MAX(SEAICE_EPS_SQ,etaDenC))
             zetaMaxC = ecc2*zetaC(I,J)
     &            *( deltaC(I,J)-( e11C(I,J)+e22C(I,J) ) )/etaDenC
             etaDenZ  = (e11Z(I,J)-e22Z(I,J))**2 + 4. _d 0*e12Z(I,J)**2
             etaDenZ  = SQRT(MAX(SEAICE_EPS_SQ,etaDenZ))
             zetaMaxZ = ecc2*zetaZ(I,J)
     &            *( deltaZ(I,J)-( e11Z(I,J)+e22Z(I,J) ) )/etaDenZ
#ifdef ALLOW_DIAGNOSTICS
C     save new eta for diagnostics
             eta(I,J,bi,bj) = MIN(zetaC(I,J),zetaMaxC)*recip_ecc2
#endif /* ALLOW_DIAGNOSTICS */
             seaice_div    (I,J) = (
     &            2. _d 0 *zetaC(I,J)*(e11C(I,J)+e22C(I,J))-pressC(I,J)
     &            ) * hEffM(I,J,bi,bj)
             seaice_tension(I,J) = 2. _d 0*MIN(zetaC(I,J),zetaMaxC)
     &            *(e11C(I,J)-e22C(I,J)) * hEffM(I,J,bi,bj)
             seaice_shear  (I,J) =
     &            2. _d 0*MIN(zetaZ(I,J),zetaMaxZ)*2. _d 0*e12Z(I,J)
            ENDDO
           ENDDO
          ELSE
#else
          IF (.TRUE. ) THEN
#endif /* SEAICE_ALLOW_TEM */
           DO j=0,sNy+1
            DO i=0,sNx+1
             seaice_div    (I,J) = (
     &            2. _d 0 *zetaC(I,J)*(e11C(I,J)+e22C(I,J))-pressC(I,J)
     &            ) * hEffM(I,J,bi,bj)
             seaice_tension(I,J) = 2. _d 0*zetaC(I,J)
     &            *(e11C(I,J)-e22C(I,J)) * hEffM(I,J,bi,bj)
             seaice_shear  (I,J) = 2. _d 0*zetaZ(I,J)*2. _d 0*e12Z(I,J)
            ENDDO
           ENDDO
          ENDIF
C
C     first step stress equations
C
          DO j=0,sNy
           DO i=0,sNx
C     sigma1 and sigma2 are computed on C points
            seaice_sigma1 (I,J,bi,bj) = ( seaice_sigma1 (I,J,bi,bj)
     &           + SEAICE_deltaTevp *  0.5 _d 0 * recip_evp_tau
     &           * seaice_div(I,J)
     &           ) * denom1
     &           *hEffM(I,J,bi,bj)
            seaice_sigma2 (I,J,bi,bj) = ( seaice_sigma2 (I,J,bi,bj)
     &           + SEAICE_deltaTevp * 0.5 _d 0 * recip_evp_tau
     &           * seaice_tension(I,J)
     &           ) * denom2
     &         *hEffM(I,J,bi,bj)
C     recover sigma11 and sigma22
            sig11(I,J) = 0.5 _d 0 *
     &           ( seaice_sigma1(I,J,bi,bj)+seaice_sigma2(I,J,bi,bj) )
            sig22(I,J) = 0.5 _d 0 *
     &           ( seaice_sigma1(I,J,bi,bj)-seaice_sigma2(I,J,bi,bj) )
           ENDDO
          ENDDO

C     sigma12 is computed on Z points
          DO j=1,sNy+1
           DO i=1,sNx+1
            seaice_sigma12(I,J,bi,bj) = ( seaice_sigma12(I,J,bi,bj)
     &           + SEAICE_deltaTevp * 0.25 _d 0 * recip_evp_tau *
     &           seaice_shear(I,J)
     &           ) * denom2
           ENDDO
          ENDDO
C
C     compute divergence of stress tensor
C
          DO J=1,sNy
           DO I=1,sNx
            stressDivergenceX(I,J,bi,bj) =
     &           ( sig11(I  ,J  ) * _dyF(I  ,J,bi,bj)
     &           - sig11(I-1,J  ) * _dyF(I-1,J,bi,bj)
     &           + seaice_sigma12(I,J+1,bi,bj) * _dxV(I,J+1,bi,bj)
     &           - seaice_sigma12(I,J  ,bi,bj) * _dxV(I,J  ,bi,bj)
     &           ) * recip_rAw(I,J,bi,bj)
            stressDivergenceY(I,J,bi,bj) =
     &           ( sig22(I,J  ) * _dxF(I,J  ,bi,bj)
     &           - sig22(I,J-1) * _dxF(I,J-1,bi,bj)
     &           + seaice_sigma12(I+1,J,bi,bj) * _dyU(I+1,J,bi,bj)
     &           - seaice_sigma12(I  ,J,bi,bj) * _dyU(I  ,J,bi,bj)
     &           ) * recip_rAs(I,J,bi,bj)
           ENDDO
          ENDDO

C
C     set up rhs for stepping the velocity field
C
          DO J=0,sNy
           DO I=0,sNx
C     set up non-linear water drag, forcex, forcey
            TEMPVAR = QUART*(
     &           ( uIceC(I  ,J,bi,bj)-uVel(I  ,J,kSrf,bi,bj)
     &           + uIceC(I+1,J,bi,bj)-uVel(I+1,J,kSrf,bi,bj))**2
     &           +(vIceC(I,J  ,bi,bj)-vVel(I,J  ,kSrf,bi,bj)
     &           + vIceC(I,J+1,bi,bj)-vVel(I,J+1,kSrf,bi,bj))**2)
            IF ( YC(I,J,bi,bj) .LT. ZERO ) THEN
             IF ( TEMPVAR .LE. (QUART/SEAICE_waterDrag_south)**2 ) THEN
              DWATN(I,J,bi,bj)=QUART
             ELSE
              DWATN(I,J,bi,bj)=SEAICE_waterDrag_south*SQRT(TEMPVAR)
             ENDIF
            ELSE
             IF ( TEMPVAR .LE. (QUART/SEAICE_waterDrag)**2 ) THEN
              DWATN(I,J,bi,bj)=QUART
             ELSE
              DWATN(I,J,bi,bj)=SEAICE_waterDrag*SQRT(TEMPVAR)
             ENDIF
            ENDIF
            DWATN(I,J,bi,bj) = DWATN(I,J,bi,bj) * HEFFM(I,J,bi,bj)
C     set up symmetric drag
            DRAGS(I,J,bi,bj) = DWATN(I,J,bi,bj)*COSWAT
           ENDDO
          ENDDO

          DO j=1,sNy
           DO i=1,sNx
C     set up anti symmetric drag force and add in ice ocean stress
C     ( remember to average to correct velocity points )
            FORCEX(I,J,bi,bj)=FORCEX0(I,J,bi,bj)+
     &           0.5 _d 0 * ( DWATN(I,J,bi,bj)+DWATN(I-1,J,bi,bj) ) *
     &           COSWAT * uVel(I,J,kSrf,bi,bj)
     &           - SIGN(SINWAT, _fCori(I,J,bi,bj))* 0.5 _d 0 *
     &           ( DWATN(I  ,J,bi,bj) * 0.5 _d 0 *
     &            (vVel(I  ,J  ,kSrf,bi,bj)-vIceC(I  ,J  ,bi,bj)
     &            +vVel(I  ,J+1,kSrf,bi,bj)-vIceC(I  ,J+1,bi,bj))
     &           + DWATN(I-1,J,bi,bj) * 0.5 _d 0 *
     &            (vVel(I-1,J  ,kSrf,bi,bj)-vIceC(I-1,J  ,bi,bj)
     &            +vVel(I-1,J+1,kSrf,bi,bj)-vIceC(I-1,J+1,bi,bj))
     &           )
            FORCEY(I,J,bi,bj)=FORCEY0(I,J,bi,bj)+
     &           0.5 _d 0 * ( DWATN(I,J,bi,bj)+DWATN(I,J-1,bi,bj) ) *
     &           COSWAT * vVel(I,J,kSrf,bi,bj)
     &           + SIGN(SINWAT, _fCori(I,J,bi,bj)) * 0.5 _d 0 *
     &           ( DWATN(I,J  ,bi,bj) * 0.5 _d 0 *
     &            (uVel(I  ,J  ,kSrf,bi,bj)-uIceC(I  ,J  ,bi,bj)
     &            +uVel(I+1,J  ,kSrf,bi,bj)-uIceC(I+1,J  ,bi,bj))
     &           + DWATN(I,J-1,bi,bj) * 0.5 _d 0 *
     &            (uVel(I  ,J-1,kSrf,bi,bj)-uIceC(I  ,J-1,bi,bj)
     &            +uVel(I+1,J-1,kSrf,bi,bj)-uIceC(I+1,J-1,bi,bj))
     &           )
C     coriols terms
            FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj) + 0.5 _d 0*(
     &             seaiceMassC(I  ,J,bi,bj) * _fCori(I  ,J,bi,bj)
     &           * 0.5 _d 0*( vIceC(I  ,J,bi,bj)+vIceC(I  ,J+1,bi,bj) )
     &           + seaiceMassC(I-1,J,bi,bj) * _fCori(I-1,J,bi,bj)
     &           * 0.5 _d 0*( vIceC(I-1,J,bi,bj)+vIceC(I-1,J+1,bi,bj) )
     &           )
            FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj) - 0.5 _d 0*(
     &             seaiceMassC(I,J  ,bi,bj) * _fCori(I,J  ,bi,bj)
     &           * 0.5 _d 0*( uIceC(I,J  ,bi,bj)+uIceC(I+1,  J,bi,bj) )
     &           + seaiceMassC(I,J-1,bi,bj) * _fCori(I,J-1,bi,bj)
     &           * 0.5 _d 0*( uIceC(I,J-1,bi,bj)+uIceC(I+1,J-1,bi,bj) )
     &           )
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
     &           + stressDivergenceX(I,J,bi,bj) )
     &           /( 1. _d 0 - seaiceMaskU(I,J,bi,bj)
     &           + seaiceMassU(I,J,bi,bj)/SEAICE_deltaTevp
     &           + 0.5 _d 0*( DRAGS(I,J,bi,bj) + DRAGS(I-1,J,bi,bj) ) )
            vIceC(I,J,bi,bj) = seaiceMaskV(I,J,bi,bj) *
     &           ( seaiceMassV(I,J,bi,bj)/SEAICE_deltaTevp
     &           * vIceC(I,J,bi,bj)
     &           + FORCEY(I,J,bi,bj)
     &           + stressDivergenceY(I,J,bi,bj) )
     &           /( 1. _d 0 - seaiceMaskV(I,J,bi,bj)
     &           + seaiceMassV(I,J,bi,bj)/SEAICE_deltaTevp
     &           + 0.5 _d 0*( DRAGS(I,J,bi,bj) + DRAGS(I,J-1,bi,bj) ) )
           ENDDO
          ENDDO
         ENDDO
        ENDDO

        CALL EXCH_UV_XY_RL(uIceC,vIceC,.TRUE.,myThid)

       ENDIF
C     end of the main time loop
      ENDDO

C     Copy work arrays and apply masks
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          uIce(I,J,bi,bj)=uIceC(I,J,bi,bj)* _maskW(I,J,kSrf,bi,bj)
          vIce(I,J,bi,bj)=vIceC(I,J,bi,bj)* _maskS(I,J,kSrf,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_DYNAMICS and SEAICE_CGRID and SEAICE_ALLOW_EVP */

      RETURN
      END
