C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_evp.F,v 1.52 2014/12/01 12:31:36 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#else
# define OBCS_UVICE_OLD
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_EVP
C     !INTERFACE:
      SUBROUTINE SEAICE_EVP( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_EVP
C     | o Ice dynamics using an EVP solver following
C     |   E. C. Hunke and J. K. Dukowicz. An
C     |   Elastic-Viscous-Plastic Model for Sea Ice Dynamics,
C     |   J. Phys. Oceanogr., 27, 1849-1867 (1997).
C     *==========================================================*
C     | written by Martin Losch, March 2006
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

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#if ( defined (SEAICE_CGRID) && \
      defined (SEAICE_ALLOW_EVP) && \
      defined (SEAICE_ALLOW_DYNAMICS) )

C     === Local variables ===
C     i,j,bi,bj      :: Loop counters
C     kSrf           :: vertical index of surface layer
C     nEVPstep       :: number of timesteps within the EVP solver
C     iEVPstep       :: Loop counter
C     SIN/COSWAT     :: sine/cosine of turning angle
C     (recip_)ecc2   :: (one over) eccentricity squared
C     recip_evpAlpha :: 1/SEAICE_evpAlpha
C     recip_deltaT   :: 1/SEAICE_deltaTdyn
C     evpStarFac     :: 1 if SEAICEuseEVPstar = .true., 0 otherwise
C     betaFac        :: SEAICE_evpBeta/SEAICE_deltaTdyn=1/SEAICE_deltaTevp
C     betaFacP1      :: betaFac + evpStarFac/SEAICE_deltaTdyn
C     e11,e12,e22    :: components of strain rate tensor
C     seaice_div     :: divergence strain rates at C-points times P
C                       /divided by Delta minus 1
C     seaice_tension :: tension    strain rates at C-points times P
C                       /divided by Delta
C     seaice_shear   :: shear      strain rates, defined at Z-points times P
C                       /divided by Delta
C     sig11, sig22   :: sum and difference of diagonal terms of stress tensor

      INTEGER i, j, bi, bj
      INTEGER kSrf
      INTEGER nEVPstep, iEVPstep
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER ikeyloc
#else
      INTEGER nEVPstepMax
#endif

      _RL COSWAT
      _RS SINWAT
      _RL ecc2, recip_ecc2, recip_evpAlpha, recip_deltaT
      _RL betaFacP1, betaFac, evpStarFac, evpRevFac, recip_evpRevFac

      _RL seaice_div    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL seaice_tension(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL seaice_shear  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig11         (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sig22         (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     auxilliary variables
      _RL ep            (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL em            (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pressC        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL zetaC         (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL deltaZ        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     _RL zetaZ         (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL deltaCreg, deltaZreg, pressZ
#ifdef SEAICE_ALLOW_TEM
      _RL etaDenC, zetaMaxC, etaDenZ, zetaMaxZ
#endif /* SEAICE_ALLOW_TEM */
#ifdef SEAICE_ALLOW_CLIPZETA
      _RL zMaxZ, zMinZ, fac
#endif /* SEAICE_ALLOW_CLIPZETA */
      _RL denom1, denom2, facZ
      _RL locMaskU, locMaskV

C     surface level
      kSrf = 1
C--   introduce turning angles
      SINWAT=SIN(SEAICE_waterTurnAngle*deg2rad)
      COSWAT=COS(SEAICE_waterTurnAngle*deg2rad)

C     abbreviation eccentricity squared
      ecc2=SEAICE_eccen**2
      recip_ecc2 = 0. _d 0
      IF ( ecc2 .NE. 0. _d 0 ) recip_ecc2=ONE/ecc2
C     determine number of internal time steps
      nEVPstep = INT(SEAICE_deltaTdyn/SEAICE_deltaTevp)
      IF ( SEAICEnEVPstarSteps.NE.UNSET_I ) nEVPstep=SEAICEnEVPstarSteps
C     SEAICE_evpAlpha = 2. * SEAICE_evpTauRelax/SEAICE_deltaTevp
      denom1 = 1. _d 0 / ( SEAICE_evpAlpha + 1. _d 0 )
      denom2 = 1. _d 0 / ( SEAICE_evpAlpha + ecc2 )

      recip_deltaT = 1. _d 0 / SEAICE_deltaTdyn
      recip_evpAlpha = 0. _d 0
      IF ( SEAICE_evpAlpha .GT. 0. _d 0 ) 
     &     recip_evpAlpha = 1. _d 0 / SEAICE_evpAlpha
      evpStarFac = 0. _d 0
      evpRevFac  = 0. _d 0
      recip_evpRevFac = 1. _d 0
      IF ( SEAICEuseEVPstar ) evpStarFac = 1. _d 0
      IF ( SEAICEuseEVPrev  ) THEN
C     the Bouillon discretization in time is less stable, because there are
C     more explicit terms
       evpRevFac       = 1. _d 0
       evpStarFac      = 1. _d 0
       recip_evpRevFac = recip_ecc2
       denom1          = 1. _d 0 / SEAICE_evpAlpha
       denom2          = denom1
      ENDIF

      betaFac   = SEAICE_evpBeta*recip_deltaT
      betaFacP1 = betaFac + evpStarFac*recip_deltaT
#ifndef ALLOW_AUTODIFF_TAMC
      nEVPstepMax = nEVPstep
#endif

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
C     use u/vIce as work arrays: copy previous time step to u/vIceNm1
          uIceNm1(I,J,bi,bj) = uIce(I,J,bi,bj)
          vIceNm1(I,J,bi,bj) = vIce(I,J,bi,bj)
C     initialise strain rates
          e11  (I,J,bi,bj)   = 0. _d 0
          e22  (I,J,bi,bj)   = 0. _d 0
          e12  (I,J,bi,bj)   = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#ifdef SEAICE_ALLOW_CLIPZETA
C     damping constraint (Hunke, J.Comp.Phys.,2001)
      IF ( SEAICE_evpDampC .GT. 0. _d 0 ) THEN
CML       fac = HALF * SEAICE_evpDampC * SEAICE_evpTauRelax
CML     &      /SEAICE_deltaTevp**2
       fac = 0.25 _d 0 * SEAICE_evpDampC * SEAICE_evpAlpha
     &      /SEAICE_deltaTevp
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           zMax (I,J,bi,bj)   = _rA(I,J,bi,bj) * fac
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
#endif /* SEAICE_ALLOW_CLIPZETA */
C
C     start of the main time loop
      DO iEVPstep = 1, nEVPstepMax
       IF (iEVPstep.LE.nEVPstep) THEN
C
#ifdef ALLOW_AUTODIFF_TAMC
        ikeyloc = iEVPstep +
     &      (ikey_dynamics-1)*nEVPstepMax
CADJ STORE uice  = comlev1_evp,
CADJ &     key = ikeyloc, byte = isbyte
CADJ STORE vice  = comlev1_evp,
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
     I       uIce, vIce,
     O       e11, e22, e12,
     I       iEVPstep, myTime, myIter, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE e11  = comlev1_evp,key = ikeyloc, byte = isbyte
CADJ STORE e12  = comlev1_evp,key = ikeyloc, byte = isbyte
CADJ STORE e22  = comlev1_evp,key = ikeyloc, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            seaice_div    (I,J) = 0. _d 0
            seaice_tension(I,J) = 0. _d 0
            seaice_shear  (I,J) = 0. _d 0
            pressC        (I,J) = 0. _d 0
            zetaC         (I,J) = 0. _d 0
            deltaZ        (I,J) = 0. _d 0
            zetaZ   (I,J,bi,bj) = 0. _d 0
            deltaC  (I,J,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            ep(i,j) = e11(i,j,bi,bj) + e22(i,j,bi,bj)
            em(i,j) = e11(i,j,bi,bj) - e22(i,j,bi,bj)
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
C     Averaging the squares gives more accurate viscous-plastic behavior
C     than squaring the averages
            deltaC(I,J,bi,bj) =
     &           ep(I,J)**2 + recip_ecc2*em(I,J)**2
     &           + recip_ecc2*
     &               ( e12(I,  J,bi,bj)**2 + e12(I+1,J+1,bi,bj)**2
     &               + e12(I+1,J,bi,bj)**2 + e12(I,  J+1,bi,bj)**2 )
            deltaZ(I,J) = facZ *
     &           ( ep(I,J  )**2 + ep(I-1,J  )**2
     &           + ep(I,J-1)**2 + ep(I-1,J-1)**2
     &           +(em(I,J  )**2 + em(I-1,J  )**2
     &            +em(I,J-1)**2 + em(I-1,J-1)**2)*recip_ecc2
     &           )
     &           + 4. _d 0*recip_ecc2*e12(I,J,bi,bj)**2
#ifdef ALLOW_AUTODIFF_TAMC
C     avoid sqrt of 0
            IF ( deltaC(I,J,bi,bj) .GT. 0. _d 0 )
     &           deltaC(I,J,bi,bj) = SQRT(deltaC(I,J,bi,bj))
            IF ( deltaZ(I,J) .GT. 0. _d 0 )
     &           deltaZ(I,J) = SQRT(deltaZ(I,J))
#else
            deltaC(I,J,bi,bj) = SQRT(deltaC(I,J,bi,bj))
            deltaZ(I,J)       = SQRT(deltaZ(I,J))
#endif /* ALLOW_AUTODIFF_TAMC */
            deltaCreg   = MAX(deltaC(I,J,bi,bj),SEAICE_EPS)
            deltaZreg   = MAX(deltaZ(I,J),SEAICE_EPS)
C     modify pressure (copied from seaice_calc_viscosities)
            zetaC(I,J)  = HALF*press0(I,J,bi,bj)/deltaCreg
            pressZ      = (deltaZ(I,J)/deltaZreg) * facZ
     &         * ( PRESS0(I,J  ,bi,bj) + PRESS0(I-1,J  ,bi,bj)
     &           + PRESS0(I,J-1,bi,bj) + PRESS0(I-1,J-1,bi,bj) )
            zetaZ(I,J,bi,bj) = HALF/deltaZreg * pressZ
           ENDDO
          ENDDO
#ifdef SEAICE_ALLOW_CLIPZETA
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE zetac            = comlev1_evp, key = ikeyloc, byte = isbyte
CADJ STORE zetaz(:,:,bi,bj) = comlev1_evp, key = ikeyloc, byte = isbyte
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
            zetaZ(I,J,bi,bj) = MAX(zMinZ,MIN(zMaxZ,zetaZ(I,J,bi,bj)))
           ENDDO
          ENDDO
#endif /* SEAICE_ALLOW_CLIPZETA */
C     recompute pressure
          DO j=0,sNy+1
           DO i=0,sNx+1
            pressC(I,J) = TWO*zetaC(I,J)*deltaC(I,J,bi,bj)
           ENDDO
          ENDDO
#ifdef ALLOW_DIAGNOSTICS
          IF ( useDiagnostics ) THEN
C     save eta, zeta, and pressure for diagnostics
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
             facZ = 0.25 _d 0
CML            facZ = 1.0 _d 0
CML     &           / MAX(1.D0,maskC(I,  J,  kSrf,bi,bj)
CML     &           +          maskC(I-1,J,  kSrf,bi,bj)
CML     &           +          maskC(I,  J-1,kSrf,bi,bj)
CML     &           +          maskC(I-1,J-1,kSrf,bi,bj) )
C     Averaging the squares gives more accurate viscous-plastic behavior
C     than squaring the averages
             etaDenC   = em(I,J)**2 +
     &            ( e12(I,  J,bi,bj)**2 + e12(I+1,J+1,bi,bj)**2
     &            + e12(I+1,J,bi,bj)**2 + e12(I,  J+1,bi,bj)**2 )
             etaDenC  = SQRT(MAX(SEAICE_EPS_SQ,etaDenC))
             zetaMaxC = ecc2*zetaC(I,J)
     &            *(deltaC(I,J,bi,bj)-ep(I,J))/etaDenC
             etaDenZ  =
     &            facZ * ( em(I,  J  )**2 + em(I-1,J-1)**2
     &                   + em(I-1,J  )**2 + em(I,  J-1)**2 )
     &            + 4. _d 0*e12(I,J,bi,bj)**2
             etaDenZ  = SQRT(MAX(SEAICE_EPS_SQ,etaDenZ))
             zetaMaxZ = ecc2*zetaZ(I,J,bi,bj) * ( deltaZ(I,J)
     &            - facZ * ( ep(I,J  ) + ep(I-1,J  )
     &                     + ep(I,J-1) + ep(I-1,J-1) )
     &            )/etaDenZ
#ifdef ALLOW_DIAGNOSTICS
C     save new eta for diagnostics
             eta(I,J,bi,bj) = MIN(zetaC(I,J),zetaMaxC)*recip_ecc2
#endif /* ALLOW_DIAGNOSTICS */
             seaice_div    (I,J) =
     &            ( 2. _d 0 *zetaC(I,J)*ep(I,J) - pressC(I,J)
     &            ) * hEffM(I,J,bi,bj)
             seaice_tension(I,J) = 2. _d 0*MIN(zetaC(I,J),zetaMaxC)
     &            * em(I,J) * hEffM(I,J,bi,bj)
             seaice_shear  (I,J) = 
     &            2. _d 0*MIN(zetaZ(I,J,bi,bj),zetaMaxZ)
     &            * 2. _d 0*e12(I,J,bi,bj)
            ENDDO
           ENDDO
          ELSE
#else
          IF (.TRUE. ) THEN
#endif /* SEAICE_ALLOW_TEM */
           DO j=0,sNy+1
            DO i=0,sNx+1
             seaice_div    (I,J) =
     &            ( 2. _d 0 *zetaC(I,J)*ep(I,J) - pressC(I,J)
     &            ) * hEffM(I,J,bi,bj)
             seaice_tension(I,J) = 2. _d 0*zetaC(I,J)
     &            * em(I,J) * hEffM(I,J,bi,bj)
             seaice_shear  (I,J) =
     &            2. _d 0*zetaZ(I,J,bi,bj)*e12(I,J,bi,bj)
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
     &           * ( SEAICE_evpAlpha - evpRevFac )
     &           + seaice_div(I,J)
     &           ) * denom1
     &           *hEffM(I,J,bi,bj)
            seaice_sigma2 (I,J,bi,bj) = ( seaice_sigma2 (I,J,bi,bj)
     &           * ( SEAICE_evpAlpha - evpRevFac )
     &           + seaice_tension(I,J)*recip_evpRevFac
     &           ) * denom2
     &         *hEffM(I,J,bi,bj)
#ifdef SEAICE_EVP_ELIMINATE_UNDERFLOWS
C     Code to avoid very small numbers that can degrade performance.
C     Many compilers can handle this more efficiently with the help of
C     a flag (copied from CICE after correspondence with Elizabeth Hunke)
            seaice_sigma1(I,J,bi,bj) = SIGN(MAX(
     &           ABS( seaice_sigma1(I,J,bi,bj) ), SEAICE_EPS ),
     &           seaice_sigma1(I,J,bi,bj) )
            seaice_sigma2(I,J,bi,bj) = SIGN(MAX(
     &           ABS( seaice_sigma2(I,J,bi,bj) ), SEAICE_EPS ),
     &           seaice_sigma2(I,J,bi,bj) )
#endif /* SEAICE_EVP_ELIMINATE_UNDERFLOWS */
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
     &           * ( SEAICE_evpAlpha - evpRevFac )
     &           + seaice_shear(I,J)*recip_evpRevFac
     &           ) * denom2
#ifdef SEAICE_EVP_ELIMINATE_UNDERFLOWS
            seaice_sigma12(I,J,bi,bj) = SIGN(MAX(
     &           ABS( seaice_sigma12(I,J,bi,bj) ), SEAICE_EPS ),
     &           seaice_sigma12(I,J,bi,bj) )
#endif /* SEAICE_EVP_ELIMINATE_UNDERFLOWS */
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
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE stressDivergenceX  = comlev1_evp,
CADJ &     key = ikeyloc, byte = isbyte
CADJ STORE stressDivergenceY = comlev1_evp,
CADJ &     key = ikeyloc, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */


#ifdef ALLOW_AUTODIFF_TAMC
#ifdef SEAICE_DYN_STABLE_ADJOINT
Cgf zero out adjoint fields to stabilize pkg/seaice dyna. adjoint
      CALL ZERO_ADJ( 1, stressDivergenceX, myThid)
      CALL ZERO_ADJ( 1, stressDivergenceY, myThid)
#endif
#endif /* ALLOW_AUTODIFF_TAMC */

C
C     set up rhs for stepping the velocity field
C
        CALL SEAICE_OCEANDRAG_COEFFS(
     I       uIce, vIce,
     O       DWATN,
     I       iEVPstep, myTime, myIter, myThid )

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO J=1,sNy
           DO I=1,sNx
C     over open water, all terms that contain sea ice mass drop out and
C     the balance is determined by the atmosphere-ice and ice-ocean stress;
C     the staggering of uIce and vIce can cause stripes in the open ocean 
C     solution when the turning angle is non-zero (SINWAT.NE.0); 
C     we mask this term here in order to avoid the stripes and because
C     over open ocean, u/vIce do not advect anything, so that the associated
C     error is small and most likely only confinded to the ice edge but may
C     propagate into the ice covered regions.
            locMaskU = seaiceMassU(I,J,bi,bj)
            locMaskV = seaiceMassV(I,J,bi,bj)
            IF ( locMaskU .NE. 0. _d 0 ) locMaskU = 1. _d 0
            IF ( locMaskV .NE. 0. _d 0 ) locMaskV = 1. _d 0
C     to recover old results replace the above lines with the line below
C           locMaskU = 1. _d 0
C           locMaskV = 1. _d 0
C     set up anti symmetric drag force and add in ice ocean stress
C     ( remember to average to correct velocity points )
            FORCEX(I,J,bi,bj)=FORCEX0(I,J,bi,bj)+
     &           0.5 _d 0 * ( DWATN(I,J,bi,bj)+DWATN(I-1,J,bi,bj) ) *
     &           COSWAT * uVel(I,J,kSrf,bi,bj)
     &           - SIGN(SINWAT, _fCori(I,J,bi,bj))* 0.5 _d 0 *
     &           ( DWATN(I  ,J,bi,bj) * 0.5 _d 0 *
     &            (vVel(I  ,J  ,kSrf,bi,bj)-vIce(I  ,J  ,bi,bj)
     &            +vVel(I  ,J+1,kSrf,bi,bj)-vIce(I  ,J+1,bi,bj))
     &           + DWATN(I-1,J,bi,bj) * 0.5 _d 0 *
     &            (vVel(I-1,J  ,kSrf,bi,bj)-vIce(I-1,J  ,bi,bj)
     &            +vVel(I-1,J+1,kSrf,bi,bj)-vIce(I-1,J+1,bi,bj))
     &           )*locMaskU
            FORCEY(I,J,bi,bj)=FORCEY0(I,J,bi,bj)+
     &           0.5 _d 0 * ( DWATN(I,J,bi,bj)+DWATN(I,J-1,bi,bj) ) *
     &           COSWAT * vVel(I,J,kSrf,bi,bj)
     &           + SIGN(SINWAT, _fCori(I,J,bi,bj)) * 0.5 _d 0 *
     &           ( DWATN(I,J  ,bi,bj) * 0.5 _d 0 *
     &            (uVel(I  ,J  ,kSrf,bi,bj)-uIce(I  ,J  ,bi,bj)
     &            +uVel(I+1,J  ,kSrf,bi,bj)-uIce(I+1,J  ,bi,bj))
     &           + DWATN(I,J-1,bi,bj) * 0.5 _d 0 *
     &            (uVel(I  ,J-1,kSrf,bi,bj)-uIce(I  ,J-1,bi,bj)
     &            +uVel(I+1,J-1,kSrf,bi,bj)-uIce(I+1,J-1,bi,bj))
     &           )*locMaskV
C     coriols terms
            FORCEX(I,J,bi,bj)=FORCEX(I,J,bi,bj) + 0.5 _d 0*(
     &             seaiceMassC(I  ,J,bi,bj) * _fCori(I  ,J,bi,bj)
     &           * 0.5 _d 0*( vIce(I  ,J,bi,bj)+vIce(I  ,J+1,bi,bj) )
     &           + seaiceMassC(I-1,J,bi,bj) * _fCori(I-1,J,bi,bj)
     &           * 0.5 _d 0*( vIce(I-1,J,bi,bj)+vIce(I-1,J+1,bi,bj) )
     &           )
            FORCEY(I,J,bi,bj)=FORCEY(I,J,bi,bj) - 0.5 _d 0*(
     &             seaiceMassC(I,J  ,bi,bj) * _fCori(I,J  ,bi,bj)
     &           * 0.5 _d 0*( uIce(I,J  ,bi,bj)+uIce(I+1,  J,bi,bj) )
     &           + seaiceMassC(I,J-1,bi,bj) * _fCori(I,J-1,bi,bj)
     &           * 0.5 _d 0*( uIce(I,J-1,bi,bj)+uIce(I+1,J-1,bi,bj) )
     &           )
           ENDDO
          ENDDO
C
C     step momentum equations with ice-ocean stress term treated implicitly
C
          DO J=1,sNy
           DO I=1,sNx
            uIce(I,J,bi,bj) = seaiceMaskU(I,J,bi,bj) *
     &           ( seaiceMassU(I,J,bi,bj)*betaFac
     &           * uIce(I,J,bi,bj)
     &           + seaiceMassU(I,J,bi,bj)*recip_deltaT*evpStarFac
     &           * uIceNm1(I,J,bi,bj)
     &           + FORCEX(I,J,bi,bj)
     &           + stressDivergenceX(I,J,bi,bj) )
     &           /( 1. _d 0 - seaiceMaskU(I,J,bi,bj)
     &           + seaiceMassU(I,J,bi,bj)*betaFacP1
     &           + 0.5 _d 0*( DWATN(I,J,bi,bj) + DWATN(I-1,J,bi,bj) )
     &           * COSWAT )
            vIce(I,J,bi,bj) = seaiceMaskV(I,J,bi,bj) *
     &           ( seaiceMassV(I,J,bi,bj)*betaFac
     &           * vIce(I,J,bi,bj)
     &           + seaiceMassV(I,J,bi,bj)*recip_deltaT*evpStarFac
     &           * vIceNm1(I,J,bi,bj)
     &           + FORCEY(I,J,bi,bj)
     &           + stressDivergenceY(I,J,bi,bj) )
     &           /( 1. _d 0 - seaiceMaskV(I,J,bi,bj)
     &           + seaiceMassV(I,J,bi,bj)*betaFacP1
     &           + 0.5 _d 0*( DWATN(I,J,bi,bj) + DWATN(I,J-1,bi,bj) )
     &           * COSWAT )
C--   to change results  of lab_sea.hb87 test exp. (only preserve 2 digits for cg2d)
c           uIce(i,j,bi,bj) = uIceNm1(i,j,bi,bj)
c    &                       +( uIce(i,j,bi,bj) - uIceNm1(i,j,bi,bj) )
c           vIce(i,j,bi,bj) = vIceNm1(i,j,bi,bj)
c    &                       +( vIce(i,j,bi,bj) - vIceNm1(i,j,bi,bj) )
           ENDDO
          ENDDO
#ifndef OBCS_UVICE_OLD
          DO j=1,sNy
           DO i=1,sNx
            locMaskU = maskInC(i,j,bi,bj)*maskInC(i-1,j,bi,bj)
            locMaskV = maskInC(i,j,bi,bj)*maskInC(i,j-1,bi,bj)
            uIce(i,j,bi,bj) = uIce(i,j,bi,bj)*locMaskU
     &                       + uIceNm1(i,j,bi,bj)*(ONE-locMaskU)
            vIce(i,j,bi,bj) = vIce(i,j,bi,bj)*locMaskV
     &                       + vIceNm1(i,j,bi,bj)*(ONE-locMaskV)
           ENDDO
          ENDDO
#endif /* OBCS_UVICE_OLD */
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uIce = comlev1_evp, key = ikeyloc, byte = isbyte
CADJ STORE vIce = comlev1_evp, key = ikeyloc, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        CALL EXCH_UV_XY_RL(uIce,vIce,.TRUE.,myThid)

       ENDIF
C     end of the main time loop
      ENDDO

#endif /* SEAICE_ALLOW_DYNAMICS and SEAICE_CGRID and SEAICE_ALLOW_EVP */

      RETURN
      END
