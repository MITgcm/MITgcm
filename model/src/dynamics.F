C $Header: /u/gcmpack/MITgcm/model/src/dynamics.F,v 1.65 2001/03/08 20:25:01 jmc Exp $
C $Name:  $

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
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_KPP
# include "KPP.h"
#endif

#ifdef ALLOW_TIMEAVE
#include "TIMEAVE_STATV.h"
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
C                              o uTrans: Zonal transport
C                              o vTrans: Meridional transport
C                              o rTrans: Vertical transport
C     maskC,maskUp             o maskC: land/water mask for tracer cells
C                              o maskUp: land/water mask for W points
C     fVer[STUV]               o fVer: Vertical flux term - note fVer
C                                      is "pipelined" in the vertical
C                                      so we need an fVer for each
C                                      variable.
C     rhoK, rhoKM1   - Density at current level, and level above 
C     phiHyd         - Hydrostatic part of the potential phiHydi.
C                      In z coords phiHydiHyd is the hydrostatic 
C                      Potential (=pressure/rho0) anomaly
C                      In p coords phiHydiHyd is the geopotential 
C                      surface height anomaly.
C     phiSurfX, - gradient of Surface potentiel (Pressure/rho, ocean)
C     phiSurfY             or geopotentiel (atmos) in X and Y direction
C     KappaRT,       - Total diffusion in vertical for T and S.
C     KappaRS          (background + spatially varying, isopycnal term).
C     iMin, iMax     - Ranges and sub-block indices on which calculations
C     jMin, jMax       are applied.
C     bi, bj
C     k, kup,        - Index for layer above and below. kup and kDown
C     kDown, km1       are switched with layer to be the appropriate 
C                      index into fVerTerm.
      _RS xA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskC   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL phiHyd  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL rhokm1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhok    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRT (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRS (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRU (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRV (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL sigmaX  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaY  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaR  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

C This is currently used by IVDC and Diagnostics
      _RL ConvectCount (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER bi, bj
      INTEGER i, j
      INTEGER k, km1, kup, kDown

Cjmc : add for phiHyd output <- but not working if multi tile per CPU
c     CHARACTER*(MAX_LEN_MBUF) suff 
c     LOGICAL  DIFFERENT_MULTIPLE
c     EXTERNAL DIFFERENT_MULTIPLE
Cjmc(end)
 
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER    isbyte
      PARAMETER( isbyte = 4 )

      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER iikey, kkey
      INTEGER maximpl
#endif /* ALLOW_AUTODIFF_TAMC */

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
C       physics, parameterizations etc...) are calculated
C         rho = rho ( theta[n], salt[n] )
C         b   = b(rho, theta)
C         K31 = K31 ( rho )
C         Gu[n] = Gu( u[n], v[n], wVel, b, ... )
C         Gv[n] = Gv( u[n], v[n], wVel, b, ... )
C         Gt[n] = Gt( theta[n], u[n], v[n], wVel, K31, ... )
C         Gs[n] = Gs( salt[n], u[n], v[n], wVel, K31, ... )
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
#endif /* ALLOW_AUTODIFF_TAMC */

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
        DO k=1,Nr
         phiHyd(i,j,k)  = 0. _d 0 
         KappaRU(i,j,k) = 0. _d 0
         KappaRV(i,j,k) = 0. _d 0
         sigmaX(i,j,k) = 0. _d 0
         sigmaY(i,j,k) = 0. _d 0
         sigmaR(i,j,k) = 0. _d 0
        ENDDO
        rhoKM1 (i,j) = 0. _d 0
        rhok   (i,j) = 0. _d 0
        maskC  (i,j) = 0. _d 0
        phiSurfX(i,j) = 0. _d 0
        phiSurfY(i,j) = 0. _d 0
       ENDDO
      ENDDO


#ifdef ALLOW_AUTODIFF_TAMC
C--   HPF directive to help TAMC
CHPF$ INDEPENDENT
#endif /* ALLOW_AUTODIFF_TAMC */

      DO bj=myByLo(myThid),myByHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
C--    HPF directive to help TAMC
CHPF$  INDEPENDENT, NEW (rTrans,fVerT,fVerS,fVerU,fVerV
CHPF$&                  ,phiHyd,utrans,vtrans,maskc,xA,yA
CHPF$&                  ,KappaRT,KappaRS,KappaRU,KappaRV
CHPF$&                  )
#endif /* ALLOW_AUTODIFF_TAMC */

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
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Set up work arrays that need valid initial values
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          rTrans(i,j)   = 0. _d 0
          fVerT (i,j,1) = 0. _d 0
          fVerT (i,j,2) = 0. _d 0
          fVerS (i,j,1) = 0. _d 0
          fVerS (i,j,2) = 0. _d 0
          fVerU (i,j,1) = 0. _d 0
          fVerU (i,j,2) = 0. _d 0
          fVerV (i,j,1) = 0. _d 0
          fVerV (i,j,2) = 0. _d 0
         ENDDO
        ENDDO

        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
C This is currently also used by IVDC and Diagnostics
           ConvectCount(i,j,k) = 0.
           KappaRT(i,j,k) = 0. _d 0
           KappaRS(i,j,k) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

        iMin = 1-OLx+1
        iMax = sNx+OLx
        jMin = 1-OLy+1
        jMax = sNy+OLy


C--     Start of diagnostic loop
        DO k=Nr,1,-1

#ifdef ALLOW_AUTODIFF_TAMC
C? Patrick, is this formula correct now that we change the loop range?
C? Do we still need this?
         kkey = (ikey-1)*(Nr-2+1) + (k-2) + 1
#endif /* ALLOW_AUTODIFF_TAMC */

C--       Integrate continuity vertically for vertical velocity
          CALL INTEGRATE_FOR_W(
     I                         bi, bj, k, uVel, vVel,
     O                         wVel,
     I                         myThid )

#ifdef    ALLOW_OBCS
#ifdef    ALLOW_NONHYDROSTATIC
C--       Apply OBC to W if in N-H mode
          IF (useOBCS.AND.nonHydrostatic) THEN
            CALL OBCS_APPLY_W( bi, bj, k, wVel, myThid )
          ENDIF
#endif    /* ALLOW_NONHYDROSTATIC */
#endif    /* ALLOW_OBCS */

C--       Calculate gradients of potential density for isoneutral
C         slope terms (e.g. GM/Redi tensor or IVDC diffusivity)
c         IF ( k.GT.1 .AND. (useGMRedi.OR.ivdc_kappa.NE.0.) ) THEN
          IF ( useGMRedi .OR. (k.GT.1 .AND. ivdc_kappa.NE.0.) ) THEN
            CALL FIND_RHO(
     I        bi, bj, iMin, iMax, jMin, jMax, k, k, eosType,
     I        theta, salt,
     O        rhoK,
     I        myThid )
            IF (k.GT.1) CALL FIND_RHO(
     I        bi, bj, iMin, iMax, jMin, jMax, k-1, k, eosType,
     I        theta, salt,
     O        rhoKm1,
     I        myThid )
            CALL GRAD_SIGMA(
     I             bi, bj, iMin, iMax, jMin, jMax, k,
     I             rhoK, rhoKm1, rhoK,
     O             sigmaX, sigmaY, sigmaR,
     I             myThid )
          ENDIF

C--       Implicit Vertical Diffusion for Convection
c ==> should use sigmaR !!!
          IF (k.GT.1 .AND. ivdc_kappa.NE.0.) THEN
            CALL CALC_IVDC(
     I        bi, bj, iMin, iMax, jMin, jMax, k,
     I        rhoKm1, rhoK,
     U        ConvectCount, KappaRT, KappaRS,
     I        myTime, myIter, myThid)
          ENDIF

C--     end of diagnostic k loop (Nr:1)
        ENDDO

#ifdef  ALLOW_OBCS
C--     Calculate future values on open boundaries
        IF (useOBCS) THEN
          CALL OBCS_CALC( bi, bj, myTime+deltaT,
     I            uVel, vVel, wVel, theta, salt,
     I            myThid )
        ENDIF
#endif  /* ALLOW_OBCS */

C--     Determines forcing terms based on external fields
C       relaxation terms, etc.
        CALL EXTERNAL_FORCING_SURF( 
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             myThid )

#ifdef  ALLOW_GMREDI
C--     Calculate iso-neutral slopes for the GM/Redi parameterisation
        IF (useGMRedi) THEN
          DO k=1,Nr
            CALL GMREDI_CALC_TENSOR(
     I             bi, bj, iMin, iMax, jMin, jMax, k,
     I             sigmaX, sigmaY, sigmaR,
     I             myThid )
          ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
        ELSE
          DO k=1, Nr
            CALL GMREDI_CALC_TENSOR_DUMMY(
     I             bi, bj, iMin, iMax, jMin, jMax, k,
     I             sigmaX, sigmaY, sigmaR,
     I             myThid )
          ENDDO
#endif /* ALLOW_AUTODIFF_TAMC */
        ENDIF
#endif  /* ALLOW_GMREDI */

#ifdef  ALLOW_KPP
C--     Compute KPP mixing coefficients
        IF (useKPP) THEN
          CALL KPP_CALC(
     I                  bi, bj, myTime, myThid )
        ENDIF
#endif  /* ALLOW_KPP */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KappaRT(:,:,:)     = comlev1_bibj, key = ikey, byte = isbyte
CADJ STORE KappaRS(:,:,:)     = comlev1_bibj, key = ikey, byte = isbyte
CADJ STORE theta(:,:,:,bi,bj) = comlev1_bibj, key = ikey, byte = isbyte
CADJ STORE salt (:,:,:,bi,bj) = comlev1_bibj, key = ikey, byte = isbyte
CADJ STORE uvel (:,:,:,bi,bj) = comlev1_bibj, key = ikey, byte = isbyte
CADJ STORE vvel (:,:,:,bi,bj) = comlev1_bibj, key = ikey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AIM
C       AIM - atmospheric intermediate model, physics package code.
C note(jmc) : phiHyd=0 at this point but is not really used in Molteni Physics
        IF ( useAIM ) THEN
         CALL TIMER_START('AIM_DO_ATMOS_PHYS      [DYNAMICS]', myThid)
         CALL AIM_DO_ATMOS_PHYSICS( phiHyd, myTime, myThid )
         CALL TIMER_STOP ('AIM_DO_ATMOS_PHYS      [DYNAMICS]', myThid)
        ENDIF
#endif /* ALLOW_AIM */


C--     Start of thermodynamics loop
        DO k=Nr,1,-1

C--       km1    Points to level above k (=k-1)
C--       kup    Cycles through 1,2 to point to layer above
C--       kDown  Cycles through 2,1 to point to current layer

          km1  = MAX(1,k-1)
          kup  = 1+MOD(k+1,2)
          kDown= 1+MOD(k,2)

          iMin = 1-OLx+2
          iMax = sNx+OLx-1
          jMin = 1-OLy+2
          jMax = sNy+OLy-1

#ifdef ALLOW_AUTODIFF_TAMC 
CPatrick Is this formula correct?
         kkey = (ikey-1)*(Nr-1+1) + (k-1) + 1
CADJ STORE rTrans(:,:)       = comlev1_bibj_k, key = kkey, byte = isbyte
CADJ STORE KappaRT(:,:,:)    = comlev1_bibj_k, key = kkey, byte = isbyte
CADJ STORE KappaRS(:,:,:)    = comlev1_bibj_k, key = kkey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C--      Get temporary terms used by tendency routines
         CALL CALC_COMMON_FACTORS (
     I        bi,bj,iMin,iMax,jMin,jMax,k,km1,kup,kDown,
     O        xA,yA,uTrans,vTrans,rTrans,maskC,maskUp,
     I        myThid)

#ifdef  INCLUDE_CALC_DIFFUSIVITY_CALL
C--      Calculate the total vertical diffusivity
         CALL CALC_DIFFUSIVITY(
     I        bi,bj,iMin,iMax,jMin,jMax,k,
     I        maskC,maskup,
     O        KappaRT,KappaRS,KappaRU,KappaRV,
     I        myThid)
#endif

C--      Calculate active tracer tendencies (gT,gS,...)
C        and step forward storing result in gTnm1, gSnm1, etc.
         IF ( tempStepping ) THEN
           CALL CALC_GT(
     I         bi,bj,iMin,iMax,jMin,jMax, k,km1,kup,kDown,
     I         xA,yA,uTrans,vTrans,rTrans,maskUp,maskC,
     I         KappaRT,
     U         fVerT,
     I         myTime, myThid)
           CALL TIMESTEP_TRACER(
     I         bi,bj,iMin,iMax,jMin,jMax,k,
     I         theta, gT,
     U         gTnm1,
     I         myIter, myThid)
         ENDIF
         IF ( saltStepping ) THEN
           CALL CALC_GS(
     I         bi,bj,iMin,iMax,jMin,jMax, k,km1,kup,kDown,
     I         xA,yA,uTrans,vTrans,rTrans,maskUp,maskC,
     I         KappaRS,
     U         fVerS,
     I         myTime, myThid)
           CALL TIMESTEP_TRACER(
     I         bi,bj,iMin,iMax,jMin,jMax,k,
     I         salt, gS,
     U         gSnm1,
     I         myIter, myThid)
         ENDIF

#ifdef   ALLOW_OBCS
C--      Apply open boundary conditions
         IF (useOBCS) THEN
           CALL OBCS_APPLY_TS( bi, bj, k, gTnm1, gSnm1, myThid )
         END IF
#endif   /* ALLOW_OBCS */

C--      Freeze water
         IF (allowFreezing) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE gTNm1(:,:,k,bi,bj) = comlev1_bibj_k
CADJ &   , key = kkey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
            CALL FREEZE( bi, bj, iMin, iMax, jMin, jMax, k, myThid )
         END IF

C--     end of thermodynamic k loop (Nr:1)
        ENDDO


#ifdef ALLOW_AUTODIFF_TAMC
CPatrick? What about this one?
           maximpl = 6
           iikey = (ikey-1)*maximpl
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Implicit diffusion
        IF (implicitDiffusion) THEN

         IF (tempStepping) THEN
#ifdef ALLOW_AUTODIFF_TAMC
            idkey = iikey + 1
#endif /* ALLOW_AUTODIFF_TAMC */
            CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTtracer, KappaRT, recip_HFacC,
     U         gTNm1,
     I         myThid )
         ENDIF

         IF (saltStepping) THEN
#ifdef ALLOW_AUTODIFF_TAMC
         idkey = iikey + 2
#endif /* ALLOW_AUTODIFF_TAMC */
            CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTtracer, KappaRS, recip_HFacC,
     U         gSNm1,
     I         myThid )
         ENDIF

#ifdef   ALLOW_OBCS
C--      Apply open boundary conditions
         IF (useOBCS) THEN
           DO K=1,Nr
             CALL OBCS_APPLY_TS( bi, bj, k, gTnm1, gSnm1, myThid )
           ENDDO
         END IF
#endif   /* ALLOW_OBCS */

C--     End If implicitDiffusion
        ENDIF

C--     Start computation of dynamics
        iMin = 1-OLx+2
        iMax = sNx+OLx-1
        jMin = 1-OLy+2
        jMax = sNy+OLy-1

C--     Explicit part of the Surface Potentiel Gradient (add in TIMESTEP)
C       (note: this loop will be replaced by CALL CALC_GRAD_ETA)
        IF (implicSurfPress.NE.1.) THEN
          CALL CALC_GRAD_PHI_SURF(
     I         bi,bj,iMin,iMax,jMin,jMax,
     I         etaN,
     O         phiSurfX,phiSurfY,
     I         myThid )                         
        ENDIF

C--     Start of dynamics loop
        DO k=1,Nr

C--       km1    Points to level above k (=k-1)
C--       kup    Cycles through 1,2 to point to layer above
C--       kDown  Cycles through 2,1 to point to current layer

          km1  = MAX(1,k-1)
          kup  = 1+MOD(k+1,2)
          kDown= 1+MOD(k,2)

C--      Integrate hydrostatic balance for phiHyd with BC of 
C        phiHyd(z=0)=0
C        distinguishe between Stagger and Non Stagger time stepping
         IF (staggerTimeStep) THEN
           CALL CALC_PHI_HYD(
     I        bi,bj,iMin,iMax,jMin,jMax,k,
     I        gTnm1, gSnm1,
     U        phiHyd,
     I        myThid )
         ELSE
           CALL CALC_PHI_HYD(
     I        bi,bj,iMin,iMax,jMin,jMax,k,
     I        theta, salt,
     U        phiHyd,
     I        myThid )
         ENDIF

C--      Calculate accelerations in the momentum equations (gU, gV, ...)
C        and step forward storing the result in gUnm1, gVnm1, etc...
         IF ( momStepping ) THEN
           CALL CALC_MOM_RHS(
     I         bi,bj,iMin,iMax,jMin,jMax,k,kup,kDown,
     I         phiHyd,KappaRU,KappaRV,
     U         fVerU, fVerV,
     I         myTime, myThid)
           CALL TIMESTEP(
     I         bi,bj,iMin,iMax,jMin,jMax,k,
     I         phiHyd, phiSurfX, phiSurfY,
     I         myIter, myThid)

#ifdef   ALLOW_OBCS
C--      Apply open boundary conditions
         IF (useOBCS) THEN
           CALL OBCS_APPLY_UV( bi, bj, k, gUnm1, gVnm1, myThid )
         END IF
#endif   /* ALLOW_OBCS */

#ifdef   ALLOW_AUTODIFF_TAMC
#ifdef   INCLUDE_CD_CODE
         ELSE
           DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
               guCD(i,j,k,bi,bj) = 0.0
               gvCD(i,j,k,bi,bj) = 0.0
             END DO
           END DO
#endif   /* INCLUDE_CD_CODE */
#endif   /* ALLOW_AUTODIFF_TAMC */
         ENDIF


C--     end of dynamics k loop (1:Nr)
        ENDDO



C--     Implicit viscosity
        IF (implicitViscosity.AND.momStepping) THEN
#ifdef    ALLOW_AUTODIFF_TAMC
          idkey = iikey + 3
#endif    /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTmom, KappaRU,recip_HFacW,
     U         gUNm1,
     I         myThid )
#ifdef    ALLOW_AUTODIFF_TAMC
          idkey = iikey + 4
#endif    /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTmom, KappaRV,recip_HFacS,
     U         gVNm1,
     I         myThid )

#ifdef   ALLOW_OBCS
C--      Apply open boundary conditions
         IF (useOBCS) THEN
           DO K=1,Nr
             CALL OBCS_APPLY_UV( bi, bj, k, gUnm1, gVnm1, myThid )
           ENDDO
         END IF
#endif   /* ALLOW_OBCS */

#ifdef    INCLUDE_CD_CODE
#ifdef    ALLOW_AUTODIFF_TAMC
          idkey = iikey + 5
#endif    /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTmom, KappaRU,recip_HFacW,
     U         vVelD,
     I         myThid )
#ifdef    ALLOW_AUTODIFF_TAMC
          idkey = iikey + 6
#endif    /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTmom, KappaRV,recip_HFacS,
     U         uVelD,
     I         myThid )
#endif    /* INCLUDE_CD_CODE */
C--     End If implicitViscosity.AND.momStepping
        ENDIF
 
Cjmc : add for phiHyd output <- but not working if multi tile per CPU
c       IF ( DIFFERENT_MULTIPLE(dumpFreq,myTime+deltaTClock,myTime) 
c    &  .AND. buoyancyRelation .eq. 'ATMOSPHERIC' ) THEN
c         WRITE(suff,'(I10.10)') myIter+1
c         CALL WRITE_FLD_XYZ_RL('PH.',suff,phiHyd,myIter+1,myThid) 
c       ENDIF
Cjmc(end)

#ifdef ALLOW_TIMEAVE
        IF (taveFreq.GT.0.) THEN 
          CALL TIMEAVE_CUMULATE(phiHydtave, phiHyd, Nr,
     I                              deltaTclock, bi, bj, myThid)
          IF (ivdc_kappa.NE.0.) THEN
            CALL TIMEAVE_CUMULATE(ConvectCountTave, ConvectCount, Nr,
     I                              deltaTclock, bi, bj, myThid)
          ENDIF
        ENDIF
#endif /* ALLOW_TIMEAVE */

       ENDDO
      ENDDO

      RETURN
      END
