C $Header: /u/gcmpack/MITgcm/model/src/dynamics.F,v 1.51 2000/06/21 20:46:31 heimbach Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE DYNAMICS(myTime, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE DYNAMICS                                      |
C     | o Controlling routine for the explicit part of the model |
C     |   dynamics.                                              |
C     |==========================================================|
C     | This routine evaluates the "dynamics" terms for each     |
C     | block of ocean in turn. Because the blocks of ocean have |
C     | overlap regions they are independent of one another.     |
C     | If terms involving lateral integrals are needed in this  |
C     | routine care will be needed. Similarly finite-difference |
C     | operations with stencils wider than the overlap region   |
C     | require special consideration.                           |
C     | Notes                                                    |
C     | =====                                                    |
C     | C*P* comments indicating place holders for which code is |
C     |      presently being developed.                          |
C     \==========================================================/
c
c     changed: Patrick Heimbach heimbach@mit.edu 6-Jun-2000
c              - computation of ikey wrong for nTx,nTy > 1
c                and/or nsx,nsy > 1: act1 and act2 were
c                mixed up.

      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "CG2D.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#endif

C     == Routine arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid - Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     == Local variables
C     xA, yA                 - Per block temporaries holding face areas
C     uTrans, vTrans, rTrans - Per block temporaries holding flow 
C                              transport
C     rVel                     o uTrans: Zonal transport
C                              o vTrans: Meridional transport
C                              o rTrans: Vertical transport
C                              o rVel:   Vertical velocity at upper and 
C                                        lower cell faces.
C     maskC,maskUp             o maskC: land/water mask for tracer cells
C                              o maskUp: land/water mask for W points
C     aTerm, xTerm, cTerm    - Work arrays for holding separate terms in
C     mTerm, pTerm,            tendency equations.
C     fZon, fMer, fVer[STUV]   o aTerm: Advection term
C                              o xTerm: Mixing term
C                              o cTerm: Coriolis term
C                              o mTerm: Metric term
C                              o pTerm: Pressure term
C                              o fZon: Zonal flux term
C                              o fMer: Meridional flux term
C                              o fVer: Vertical flux term - note fVer
C                                      is "pipelined" in the vertical
C                                      so we need an fVer for each
C                                      variable.
C     rhoK, rhoKM1   - Density at current level, level above and level 
C                      below.
C     rhoKP1                                                                  
C     buoyK, buoyKM1 - Buoyancy at current level and level above.
C     phiHyd         - Hydrostatic part of the potential phiHydi.
C                      In z coords phiHydiHyd is the hydrostatic 
C                      pressure anomaly
C                      In p coords phiHydiHyd is the geopotential 
C                      surface height 
C                      anomaly.
C     etaSurfX,      - Holds surface elevation gradient in X and Y.
C     etaSurfY
C     KappaRT,       - Total diffusion in vertical for T and S.
C     KappaRS          (background + spatially varying, isopycnal term).
C     iMin, iMax     - Ranges and sub-block indices on which calculations
C     jMin, jMax       are applied.
C     bi, bj
C     k, kUp,        - Index for layer above and below. kUp and kDown
C     kDown, kM1       are switched with layer to be the appropriate 
C                      index into fVerTerm.
      _RS xA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rVel    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RS maskC   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL aTerm   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL xTerm   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL cTerm   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL mTerm   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL pTerm   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fZon    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL phiHyd  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL rhokm1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhokp1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhok    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL buoyKM1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL buoyK   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhotmp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL etaSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL etaSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRT (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRS (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRU (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRV (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL sigmaX  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaY  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaR  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#ifdef INCLUDE_CONVECT_CALL
      _RL ConvectCount (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif

      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER bi, bj
      INTEGER i, j
      INTEGER k, kM1, kUp, kDown
      LOGICAL BOTTOM_LAYER

#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER    isbyte
      PARAMETER( isbyte = 4 )

      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER iikey, kkey
      INTEGER maximpl
#endif

C---    The algorithm...
C
C       "Correction Step"
C       =================
C       Here we update the horizontal velocities with the surface
C       pressure such that the resulting flow is either consistent
C       with the free-surface evolution or the rigid-lid:
C         U[n] = U* + dt x d/dx P
C         V[n] = V* + dt x d/dy P
C
C       "Calculation of Gs"
C       ===================
C       This is where all the accelerations and tendencies (ie.
C       phiHydysics, parameterizations etc...) are calculated
C         rVel = sum_r ( div. u[n] )
C         rho = rho ( theta[n], salt[n] )
C         b   = b(rho, theta)
C         K31 = K31 ( rho )
C         Gu[n] = Gu( u[n], v[n], rVel, b, ... )
C         Gv[n] = Gv( u[n], v[n], rVel, b, ... )
C         Gt[n] = Gt( theta[n], u[n], v[n], rVel, K31, ... )
C         Gs[n] = Gs( salt[n], u[n], v[n], rVel, K31, ... )
C
C       "Time-stepping" or "Prediction"
C       ================================
C       The models variables are stepped forward with the appropriate
C       time-stepping scheme (currently we use Adams-Bashforth II)
C       - For momentum, the result is always *only* a "prediction"
C       in that the flow may be divergent and will be "corrected"
C       later with a surface pressure gradient.
C       - Normally for tracers the result is the new field at time
C       level [n+1} *BUT* in the case of implicit diffusion the result
C       is also *only* a prediction.
C       - We denote "predictors" with an asterisk (*).
C         U* = U[n] + dt x ( 3/2 Gu[n] - 1/2 Gu[n-1] )
C         V* = V[n] + dt x ( 3/2 Gv[n] - 1/2 Gv[n-1] )
C         theta[n+1] = theta[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C         salt[n+1] = salt[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C       With implicit diffusion:
C         theta* = theta[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C         salt* = salt[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C         (1 + dt * K * d_zz) theta[n] = theta*
C         (1 + dt * K * d_zz) salt[n] = salt*
C---

#ifdef ALLOW_AUTODIFF_TAMC
C--   dummy statement to end declaration part
      ikey = 1
#endif

C--   Set up work arrays with valid (i.e. not NaN) values
C     These inital values do not alter the numerical results. They
C     just ensure that all memory references are to valid floating
C     point numbers. This prevents spurious hardware signals due to
C     uninitialised but inert locations.
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        xA(i,j)      = 0. _d 0
        yA(i,j)      = 0. _d 0
        uTrans(i,j)  = 0. _d 0
        vTrans(i,j)  = 0. _d 0
        aTerm(i,j)   = 0. _d 0
        xTerm(i,j)   = 0. _d 0
        cTerm(i,j)   = 0. _d 0
        mTerm(i,j)   = 0. _d 0
        pTerm(i,j)   = 0. _d 0
        fZon(i,j)    = 0. _d 0
        fMer(i,j)    = 0. _d 0
        DO K=1,Nr
         phiHyd (i,j,k)  = 0. _d 0
         KappaRU(i,j,k) = 0. _d 0
         KappaRV(i,j,k) = 0. _d 0
         sigmaX(i,j,k) = 0. _d 0
         sigmaY(i,j,k) = 0. _d 0
         sigmaR(i,j,k) = 0. _d 0
        ENDDO
        rhoKM1 (i,j) = 0. _d 0
        rhok   (i,j) = 0. _d 0
        rhoKP1 (i,j) = 0. _d 0
        rhoTMP (i,j) = 0. _d 0
        buoyKM1(i,j) = 0. _d 0
        buoyK  (i,j) = 0. _d 0
        maskC  (i,j) = 0. _d 0
       ENDDO
      ENDDO


#ifdef ALLOW_AUTODIFF_TAMC
C--   HPF directive to help TAMC
!HPF$ INDEPENDENT
#endif

      DO bj=myByLo(myThid),myByHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
C--    HPF directive to help TAMC
!HPF$  INDEPENDENT, NEW (rTrans,rVel,fVerT,fVerS,fVerU,fVerV
!HPF$&                  ,phiHyd,
!HPF$&                  ,utrans,vtrans,maskc,xA,yA
!HPF$&                  ,KappaRT,KappaRS,KappaRU,KappaRV
!HPF$&                  )
#endif

       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1

          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1

          act3 = myThid - 1
          max3 = nTx*nTy

          act4 = ikey_dynamics - 1

          ikey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif

C--     Set up work arrays that need valid initial values
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          rTrans(i,j)   = 0. _d 0
          rVel  (i,j,1) = 0. _d 0
          rVel  (i,j,2) = 0. _d 0
          fVerT (i,j,1) = 0. _d 0
          fVerT (i,j,2) = 0. _d 0
          fVerS (i,j,1) = 0. _d 0
          fVerS (i,j,2) = 0. _d 0
          fVerU (i,j,1) = 0. _d 0
          fVerU (i,j,2) = 0. _d 0
          fVerV (i,j,1) = 0. _d 0
          fVerV (i,j,2) = 0. _d 0
          phiHyd(i,j,1) = 0. _d 0
         ENDDO
        ENDDO

        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
#ifdef INCLUDE_CONVECT_CALL
           ConvectCount(i,j,k) = 0.
#endif
           KappaRT(i,j,k) = 0. _d 0
           KappaRS(i,j,k) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

        iMin = 1-OLx+1
        iMax = sNx+OLx
        jMin = 1-OLy+1
        jMax = sNy+OLy


        K = 1
        BOTTOM_LAYER = K .EQ. Nr

#ifdef DO_PIPELINED_CORRECTION_STEP
C--     Calculate gradient of surface pressure
        CALL CALC_GRAD_ETA_SURF(
     I       bi,bj,iMin,iMax,jMin,jMax,
     O       etaSurfX,etaSurfY,
     I       myThid)
C--     Update fields in top level according to tendency terms
        CALL CORRECTION_STEP(
     I       bi,bj,iMin,iMax,jMin,jMax,K,
     I       etaSurfX,etaSurfY,myTime,myThid)

#ifdef ALLOW_OBCS
        IF (openBoundaries) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uvel (:,:,k,bi,bj)  = comlev1_2d, key = ikey, byte = isbyte
CADJ STORE vvel (:,:,k,bi,bj)  = comlev1_2d, key = ikey, byte = isbyte
CADJ STORE theta(:,:,k,bi,bj)  = comlev1_2d, key = ikey, byte = isbyte
CADJ STORE salt(:,:,k,bi,bj)   = comlev1_2d, key = ikey, byte = isbyte
#endif
           CALL APPLY_OBCS1( bi, bj, K, myThid )
        END IF
#endif

        IF ( .NOT. BOTTOM_LAYER ) THEN
C--      Update fields in layer below according to tendency terms
         CALL CORRECTION_STEP(
     I        bi,bj,iMin,iMax,jMin,jMax,K+1,
     I        etaSurfX,etaSurfY,myTime,myThid)
#ifdef ALLOW_OBCS
         IF (openBoundaries) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uvel (:,:,k,bi,bj)  = comlev1_2d, key = ikey, byte = isbyte
CADJ STORE vvel (:,:,k,bi,bj)  = comlev1_2d, key = ikey, byte = isbyte
CADJ STORE theta(:,:,k,bi,bj)  = comlev1_2d, key = ikey, byte = isbyte
CADJ STORE salt(:,:,k,bi,bj)   = comlev1_2d, key = ikey, byte = isbyte
#endif
            CALL APPLY_OBCS1( bi, bj, K+1, myThid )
         END IF
#endif
        ENDIF
#endif
C--     Density of 1st level (below W(1)) reference to level 1
#ifdef  INCLUDE_FIND_RHO_CALL
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,k,bi,bj)  = comlev1_2d, key = ikey, byte = isbyte
CADJ STORE salt (:,:,k,bi,bj)  = comlev1_2d, key = ikey, byte = isbyte
#endif
        CALL FIND_RHO(
     I     bi, bj, iMin, iMax, jMin, jMax, K, K, eosType,
     O     rhoKm1,
     I     myThid )
#endif

        IF (       (.NOT. BOTTOM_LAYER)
     &     ) THEN
C--      Check static stability with layer below
C--      and mix as needed.
#ifdef  INCLUDE_FIND_RHO_CALL
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,k,bi,bj)  = comlev1_2d, key = ikey, byte = isbyte
CADJ STORE salt (:,:,k,bi,bj)  = comlev1_2d, key = ikey, byte = isbyte
#endif
         CALL FIND_RHO(
     I      bi, bj, iMin, iMax, jMin, jMax, K+1, K, eosType,
     O      rhoKp1,
     I      myThid )
#endif

#ifdef  INCLUDE_CONVECT_CALL

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rhoKm1(:,:)  = comlev1_2d, key = ikey, byte = isbyte
CADJ STORE rhoKp1(:,:)  = comlev1_2d, key = ikey, byte = isbyte
#endif
         CALL CONVECT(
     I       bi,bj,iMin,iMax,jMin,jMax,K+1,rhoKm1,rhoKp1,
     U       ConvectCount,
     I       myTime,myIter,myThid)
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,k+1,bi,bj),theta(:,:,k,bi,bj)
CADJ &     = comlev1_2d, key = ikey, byte = isbyte
CADJ STORE salt (:,:,k+1,bi,bj),salt (:,:,k,bi,bj)
CADJ &     = comlev1_2d, key = ikey, byte = isbyte
#endif

#endif

C--      Implicit Vertical Diffusion for Convection
         IF (ivdc_kappa.NE.0.) CALL CALC_IVDC(
     I       bi,bj,iMin,iMax,jMin,jMax,K+1,rhoKm1,rhoKp1,
     U       ConvectCount, KappaRT, KappaRS,
     I       myTime,myIter,myThid)
CRG: do we need do store STORE KappaRT, KappaRS ?

C--      Recompute density after mixing
#ifdef  INCLUDE_FIND_RHO_CALL
         CALL FIND_RHO(
     I      bi, bj, iMin, iMax, jMin, jMax, K, K, eosType,
     O      rhoKm1,
     I      myThid )
#endif
        ENDIF
C--     Calculate buoyancy
        CALL CALC_BUOYANCY(
     I      bi,bj,iMin,iMax,jMin,jMax,K,rhoKm1,
     O      buoyKm1,
     I      myThid )
C--     Integrate hydrostatic balance for phiHyd with BC of
C--     phiHyd(z=0)=0
        CALL CALC_PHI_HYD(
     I      bi,bj,iMin,iMax,jMin,jMax,K,buoyKm1,buoyKm1,
     U      phiHyd,
     I      myThid )
        CALL GRAD_SIGMA(
     I            bi, bj, iMin, iMax, jMin, jMax, K,
     I            rhoKm1, rhoKm1, rhoKm1,
     O            sigmaX, sigmaY, sigmaR,
     I            myThid )

C--     Start of downward loop
        DO K=2,Nr

#ifdef ALLOW_AUTODIFF_TAMC
         kkey = (ikey-1)*(Nr-2+1) + (k-2) + 1
#endif

         BOTTOM_LAYER = K .EQ. Nr

#ifdef DO_PIPELINED_CORRECTION_STEP
         IF ( .NOT. BOTTOM_LAYER ) THEN
C--       Update fields in layer below according to tendency terms
          CALL CORRECTION_STEP(
     I         bi,bj,iMin,iMax,jMin,jMax,K+1,
     I         etaSurfX,etaSurfY,myTime,myThid)
#ifdef ALLOW_OBCS
          IF (openBoundaries) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uvel (:,:,k,bi,bj)  = comlev1_3d, key = kkey, byte = isbyte
CADJ STORE vvel (:,:,k,bi,bj)  = comlev1_3d, key = kkey, byte = isbyte
CADJ STORE theta(:,:,k,bi,bj)  = comlev1_3d, key = kkey, byte = isbyte
CADJ STORE salt(:,:,k,bi,bj)   = comlev1_2d, key = ikey, byte = isbyte
#endif
             CALL APPLY_OBCS1( bi, bj, K+1, myThid )
          END IF
#endif
         ENDIF
#endif

C--      Density of K level (below W(K)) reference to K level
#ifdef  INCLUDE_FIND_RHO_CALL
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,k,bi,bj)  = comlev1_3d, key = kkey, byte = isbyte
CADJ STORE salt (:,:,k,bi,bj)  = comlev1_3d, key = kkey, byte = isbyte
#endif
         CALL FIND_RHO(
     I      bi, bj, iMin, iMax, jMin, jMax,  K, K, eosType,
     O      rhoK,
     I      myThid )
#endif
         IF (       (.NOT. BOTTOM_LAYER)
     &      ) THEN
C--       Check static stability with layer below and mix as needed.
C--       Density of K+1 level (below W(K+1)) reference to K level.
#ifdef  INCLUDE_FIND_RHO_CALL
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,k,bi,bj)  = comlev1_3d, key = kkey, byte = isbyte
CADJ STORE salt (:,:,k,bi,bj)  = comlev1_3d, key = kkey, byte = isbyte
#endif
          CALL FIND_RHO(
     I       bi, bj, iMin, iMax, jMin, jMax,  K+1, K, eosType,
     O       rhoKp1,
     I       myThid )
#endif

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rhok  (:,:)   = comlev1_3d, key = kkey, byte = isbyte
CADJ STORE rhoKm1(:,:)   = comlev1_3d, key = kkey, byte = isbyte
CADJ STORE rhoKp1(:,:)   = comlev1_3d, key = kkey, byte = isbyte
#endif

#ifdef  INCLUDE_CONVECT_CALL
          CALL CONVECT(
     I        bi,bj,iMin,iMax,jMin,jMax,K+1,rhoK,rhoKp1,
     U        ConvectCount,
     I        myTime,myIter,myThid)
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,k+1,bi,bj),theta(:,:,k,bi,bj)
CADJ &     = comlev1_3d, key = kkey, byte = isbyte
CADJ STORE salt (:,:,k+1,bi,bj),salt (:,:,k,bi,bj)
CADJ &     = comlev1_3d, key = kkey, byte = isbyte
#endif
#endif

C--      Implicit Vertical Diffusion for Convection
         IF (ivdc_kappa.NE.0.) THEN
            CALL CALC_IVDC(
     I       bi,bj,iMin,iMax,jMin,jMax,K+1,rhoKm1,rhoKp1,
     U       ConvectCount, KappaRT, KappaRS,
     I       myTime,myIter,myThid)
CRG: do we need do store STORE KappaRT, KappaRS ?
         END IF

C--       Recompute density after mixing
#ifdef  INCLUDE_FIND_RHO_CALL
          CALL FIND_RHO(
     I       bi, bj, iMin, iMax, jMin, jMax, K, K, eosType,
     O       rhoK,
     I       myThid )
#endif
         ENDIF
C--      Calculate buoyancy
         CALL CALC_BUOYANCY(
     I       bi,bj,iMin,iMax,jMin,jMax,K,rhoK,
     O       buoyK,
     I       myThid )
C--      Integrate hydrostatic balance for phiHyd with BC of 
C--      phiHyd(z=0)=0
         CALL CALC_PHI_HYD(
     I        bi,bj,iMin,iMax,jMin,jMax,K,buoyKm1,buoyK,
     U        phiHyd,
     I        myThid )
C--      Calculate iso-neutral slopes for the GM/Redi parameterisation
#ifdef  INCLUDE_FIND_RHO_CALL
         CALL FIND_RHO(
     I        bi, bj, iMin, iMax, jMin, jMax, K-1, K, eosType,
     O        rhoTmp,
     I        myThid )
#endif
         CALL GRAD_SIGMA(
     I             bi, bj, iMin, iMax, jMin, jMax, K,
     I             rhoK, rhotmp, rhoK,
     O             sigmaX, sigmaY, sigmaR,
     I             myThid )


         DO J=jMin,jMax
          DO I=iMin,iMax
#ifdef  INCLUDE_FIND_RHO_CALL
           rhoKm1 (I,J) = rhoK(I,J)
#endif
           buoyKm1(I,J) = buoyK(I,J)
          ENDDO
         ENDDO
        ENDDO
C--     end of k loop

#ifdef ALLOW_GMREDI
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rhoTmp(:,:)  = comlev1_3d, key = kkey, byte = isbyte
CADJ STORE rhok  (:,:)  = comlev1_3d, key = kkey, byte = isbyte
CADJ STORE rhoKm1(:,:)  = comlev1_3d, key = kkey, byte = isbyte
#endif
        DO K=1, Nr
         IF (use_GMRedi) CALL GMREDI_CALC_TENSOR(
     I             bi, bj, iMin, iMax, jMin, jMax, K,
     I             sigmaX, sigmaY, sigmaR,
     I             myThid )
        ENDDO
#endif

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,:,bi,bj)  = comlev1_2d, key = ikey, byte = isbyte
CADJ STORE salt (:,:,:,bi,bj)  = comlev1_2d, key = ikey, byte = isbyte
CADJ STORE uvel (:,:,:,bi,bj)  = comlev1_2d, key = ikey, byte = isbyte
CADJ STORE vvel (:,:,:,bi,bj)  = comlev1_2d, key = ikey, byte = isbyte
#endif

#ifdef ALLOW_KPP
C--     Compute KPP mixing coefficients
        CALL TIMER_START('KPP_CALC               [DYNAMICS]', myThid)
        CALL KPP_CALC(
     I               bi, bj, myTime, myThid )
        CALL TIMER_STOP ('KPP_CALC               [DYNAMICS]', myThid)
#endif

C--     Start of upward loop
        DO K = Nr, 1, -1

         kM1  =max(1,k-1)   ! Points to level above k (=k-1)
         kUp  =1+MOD(k+1,2) ! Cycles through 1,2 to point to layer above
         kDown=1+MOD(k,2)   ! Cycles through 2,1 to point to current layer

         iMin = 1-OLx+2
         iMax = sNx+OLx-1
         jMin = 1-OLy+2
         jMax = sNy+OLy-1

#ifdef ALLOW_AUTODIFF_TAMC 
         kkey = (ikey-1)*(Nr-1+1) + (k-1) + 1
#endif

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rvel  (:,:,kDown)  = comlev1_3d, key = kkey, byte = isbyte
CADJ STORE rTrans(:,:)        = comlev1_3d, key = kkey, byte = isbyte
CADJ STORE KappaRT(:,:,:)     = comlev1_3d, key = kkey, byte = isbyte
CADJ STORE KappaRS(:,:,:)     = comlev1_3d, key = kkey, byte = isbyte
#endif

C--      Get temporary terms used by tendency routines
         CALL CALC_COMMON_FACTORS (
     I        bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     O        xA,yA,uTrans,vTrans,rTrans,rVel,maskC,maskUp,
     I        myThid)

#ifdef ALLOW_OBCS
        IF (openBoundaries) THEN
         CALL APPLY_OBCS3( bi, bj, K, Kup, rTrans, rVel, myThid )
        ENDIF
#endif

#ifdef  INCLUDE_CALC_DIFFUSIVITY_CALL
C--      Calculate the total vertical diffusivity
         CALL CALC_DIFFUSIVITY(
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     I        maskC,maskUp,
     O        KappaRT,KappaRS,KappaRU,KappaRV,
     I        myThid)
#endif
C--      Calculate accelerations in the momentum equations
         IF ( momStepping ) THEN
          CALL CALC_MOM_RHS(
     I         bi,bj,iMin,iMax,jMin,jMax,k,kM1,kUp,kDown,
     I         xA,yA,uTrans,vTrans,rTrans,rVel,maskC,
     I         phiHyd,KappaRU,KappaRV,
     U         aTerm,xTerm,cTerm,mTerm,pTerm,
     U         fZon, fMer, fVerU, fVerV,
     I         myTime, myThid)
#ifdef ALLOW_AUTODIFF_TAMC
#ifdef INCLUDE_CD_CODE
         ELSE
            DO j=1-OLy,sNy+OLy
               DO i=1-OLx,sNx+OLx
                  guCD(i,j,k,bi,bj) = 0.0
                  gvCD(i,j,k,bi,bj) = 0.0
               END DO
            END DO
#endif
#endif
         ENDIF
C--      Calculate active tracer tendencies
         IF ( tempStepping ) THEN
          CALL CALC_GT(
     I         bi,bj,iMin,iMax,jMin,jMax, k,kM1,kUp,kDown,
     I         xA,yA,uTrans,vTrans,rTrans,maskUp,maskC,
     I         KappaRT,
     U         aTerm,xTerm,fZon,fMer,fVerT,
     I         myTime, myThid)
         ENDIF
         IF ( saltStepping ) THEN
          CALL CALC_GS(
     I         bi,bj,iMin,iMax,jMin,jMax, k,kM1,kUp,kDown,
     I         xA,yA,uTrans,vTrans,rTrans,maskUp,maskC,
     I         KappaRS,
     U         aTerm,xTerm,fZon,fMer,fVerS,
     I         myTime, myThid)
         ENDIF
#ifdef ALLOW_OBCS
C--      Calculate future values on open boundaries
         IF (openBoundaries) THEN
Caja      CALL CYCLE_OBCS( K, bi, bj, myThid )
          CALL SET_OBCS( K, bi, bj, myTime+deltaTclock, myThid )
         ENDIF
#endif
C--      Prediction step (step forward all model variables)
         CALL TIMESTEP(
     I       bi,bj,iMin,iMax,jMin,jMax,K,
     I       myIter, myThid)
#ifdef ALLOW_OBCS
C--      Apply open boundary conditions
         IF (openBoundaries) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE gunm1(:,:,k,bi,bj)  = comlev1_3d, key = kkey, byte = isbyte
CADJ STORE gvnm1(:,:,k,bi,bj)  = comlev1_3d, key = kkey, byte = isbyte
CADJ STORE gwnm1(:,:,k,bi,bj)  = comlev1_3d, key = kkey, byte = isbyte
#endif
            CALL APPLY_OBCS2( bi, bj, K, myThid )
         END IF
#endif
C--      Freeze water
         IF (allowFreezing) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE gTNm1(:,:,k,bi,bj)  = comlev1_3d, key = kkey, byte = isbyte
#endif
            CALL FREEZE( bi, bj, iMin, iMax, jMin, jMax, K, myThid )
         END IF

#ifdef DIVG_IN_DYNAMICS
C--      Diagnose barotropic divergence of predicted fields
         CALL CALC_DIV_GHAT(
     I       bi,bj,iMin,iMax,jMin,jMax,K,
     I       xA,yA,
     I       myThid)
#endif /* DIVG_IN_DYNAMICS */

C--      Cumulative diagnostic calculations (ie. time-averaging)
#ifdef INCLUDE_DIAGNOSTICS_INTERFACE_CODE
         IF (taveFreq.GT.0.) THEN
          CALL DO_TIME_AVERAGES(
     I                           myTime, myIter, bi, bj, K, kUp, kDown,
     I                           rVel, ConvectCount,
     I                           myThid )
         ENDIF
#endif


        ENDDO ! K

#ifdef ALLOW_AUTODIFF_TAMC
           maximpl = 6
           iikey = (ikey-1)*maximpl
#endif

C--     Implicit diffusion
        IF (implicitDiffusion) THEN

         IF (tempStepping) THEN
#ifdef ALLOW_AUTODIFF_TAMC
            idkey = iikey + 1
#endif
            CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTtracer, KappaRT,recip_HFacC,
     U         gTNm1,
     I         myThid )
         END IF

         IF (saltStepping) THEN
#ifdef ALLOW_AUTODIFF_TAMC
         idkey = iikey + 2
#endif
            CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTtracer, KappaRS,recip_HFacC,
     U         gSNm1,
     I         myThid )
         END IF

        ENDIF ! implicitDiffusion

C--     Implicit viscosity
        IF (implicitViscosity) THEN

         IF (momStepping) THEN
#ifdef ALLOW_AUTODIFF_TAMC
         idkey = iikey + 3
#endif
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTmom, KappaRU,recip_HFacW,
     U         gUNm1,
     I         myThid )
#ifdef ALLOW_AUTODIFF_TAMC
         idkey = iikey + 4
#endif
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTmom, KappaRV,recip_HFacS,
     U         gVNm1,
     I         myThid )

#ifdef INCLUDE_CD_CODE

#ifdef ALLOW_AUTODIFF_TAMC
         idkey = iikey + 5
#endif
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTmom, KappaRU,recip_HFacW,
     U         vVelD,
     I         myThid )
#ifdef ALLOW_AUTODIFF_TAMC
        idkey = iikey + 6
#endif
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTmom, KappaRV,recip_HFacS,
     U         uVelD,
     I         myThid )

#endif

         ENDIF ! momStepping
        ENDIF ! implicitViscosity
 
       ENDDO
      ENDDO

C     write(0,*) 'dynamics: pS ',minval(cg2d_x(1:sNx,1:sNy,:,:)),
C    &                           maxval(cg2d_x(1:sNx,1:sNy,:,:))
C     write(0,*) 'dynamics: U  ',minval(uVel(1:sNx,1:sNy,1,:,:),mask=uVel(1:sNx,1:sNy,1,:,:).NE.0.),
C    &                           maxval(uVel(1:sNx,1:sNy,1,:,:),mask=uVel(1:sNx,1:sNy,1,:,:).NE.0.)
C     write(0,*) 'dynamics: V  ',minval(vVel(1:sNx,1:sNy,1,:,:),mask=vVel(1:sNx,1:sNy,1,:,:).NE.0.),
C    &                           maxval(vVel(1:sNx,1:sNy,1,:,:),mask=vVel(1:sNx,1:sNy,1,:,:).NE.0.)
C     write(0,*) 'dynamics: rVel(1) ',
C    &            minval(rVel(1:sNx,1:sNy,1),mask=rVel(1:sNx,1:sNy,1).NE.0.),
C    &            maxval(rVel(1:sNx,1:sNy,1),mask=rVel(1:sNx,1:sNy,1).NE.0.)
C     write(0,*) 'dynamics: rVel(2) ',
C    &            minval(rVel(1:sNx,1:sNy,2),mask=rVel(1:sNx,1:sNy,2).NE.0.),
C    &            maxval(rVel(1:sNx,1:sNy,2),mask=rVel(1:sNx,1:sNy,2).NE.0.)
C     write(0,*) 'dynamics: gT ',minval(gT(1:sNx,1:sNy,:,:,:)),
C    &                           maxval(gT(1:sNx,1:sNy,:,:,:))
C     write(0,*) 'dynamics: T  ',minval(Theta(1:sNx,1:sNy,:,:,:)),
C    &                           maxval(Theta(1:sNx,1:sNy,:,:,:))
C     write(0,*) 'dynamics: gS ',minval(gS(1:sNx,1:sNy,:,:,:)),
C    &                           maxval(gS(1:sNx,1:sNy,:,:,:))
C     write(0,*) 'dynamics: S  ',minval(salt(1:sNx,1:sNy,:,:,:)),
C    &                           maxval(salt(1:sNx,1:sNy,:,:,:))
C     write(0,*) 'dynamics: phiHyd ',minval(phiHyd/(Gravity*Rhonil),mask=phiHyd.NE.0.),
C    &                           maxval(phiHyd/(Gravity*Rhonil))
C     CALL PLOT_FIELD_XYZRL( gU, ' GU exiting dyanmics ' , 
C    &Nr, 1, myThid )
C     CALL PLOT_FIELD_XYZRL( gV, ' GV exiting dyanmics ' , 
C    &Nr, 1, myThid )
C     CALL PLOT_FIELD_XYZRL( gS, ' GS exiting dyanmics ' , 
C    &Nr, 1, myThid )
C     CALL PLOT_FIELD_XYZRL( gT, ' GT exiting dyanmics ' , 
C    &Nr, 1, myThid )
C     CALL PLOT_FIELD_XYZRL( phiHyd, ' phiHyd exiting dyanmics ' , 
C    &Nr, 1, myThid )


      RETURN
      END
