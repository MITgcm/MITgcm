C $Header: /u/gcmpack/MITgcm/model/src/thermodynamics.F,v 1.13 2001/09/28 03:36:16 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: THERMODYNAMICS
C     !INTERFACE:
      SUBROUTINE THERMODYNAMICS(myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE THERMODYNAMICS                                 
C     | o Controlling routine for the prognostic part of the      
C     |   thermo-dynamics.                                        
C     *===========================================================
C     | The algorithm...
C     |
C     | "Correction Step"
C     | =================
C     | Here we update the horizontal velocities with the surface
C     | pressure such that the resulting flow is either consistent
C     | with the free-surface evolution or the rigid-lid:
C     |   U[n] = U* + dt x d/dx P
C     |   V[n] = V* + dt x d/dy P
C     |
C     | "Calculation of Gs"
C     | ===================
C     | This is where all the accelerations and tendencies (ie.
C     | physics, parameterizations etc...) are calculated
C     |   rho = rho ( theta[n], salt[n] )
C     |   b   = b(rho, theta)
C     |   K31 = K31 ( rho )
C     |   Gu[n] = Gu( u[n], v[n], wVel, b, ... )
C     |   Gv[n] = Gv( u[n], v[n], wVel, b, ... )
C     |   Gt[n] = Gt( theta[n], u[n], v[n], wVel, K31, ... )
C     |   Gs[n] = Gs( salt[n], u[n], v[n], wVel, K31, ... )
C     |
C     | "Time-stepping" or "Prediction"
C     | ================================
C     | The models variables are stepped forward with the appropriate
C     | time-stepping scheme (currently we use Adams-Bashforth II)
C     | - For momentum, the result is always *only* a "prediction"
C     | in that the flow may be divergent and will be "corrected"
C     | later with a surface pressure gradient.
C     | - Normally for tracers the result is the new field at time
C     | level [n+1} *BUT* in the case of implicit diffusion the result
C     | is also *only* a prediction.
C     | - We denote "predictors" with an asterisk (*).
C     |   U* = U[n] + dt x ( 3/2 Gu[n] - 1/2 Gu[n-1] )
C     |   V* = V[n] + dt x ( 3/2 Gv[n] - 1/2 Gv[n-1] )
C     |   theta[n+1] = theta[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C     |   salt[n+1] = salt[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C     | With implicit diffusion:
C     |   theta* = theta[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C     |   salt* = salt[n] + dt x ( 3/2 Gt[n] - 1/2 atG[n-1] )
C     |   (1 + dt * K * d_zz) theta[n] = theta*
C     |   (1 + dt * K * d_zz) salt[n] = salt*
C     |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "GAD.h"
#ifdef ALLOW_PASSIVE_TRACER
#include "TR1.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
# include "FFIELDS.h"
# ifdef ALLOW_KPP
#  include "KPP.h"
# endif
# ifdef ALLOW_GMREDI
#  include "GMREDI.h"
# endif
#endif /* ALLOW_AUTODIFF_TAMC */
#ifdef ALLOW_TIMEAVE
#include "TIMEAVE_STATV.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid - Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables
C     xA, yA                 - Per block temporaries holding face areas
C     uTrans, vTrans, rTrans - Per block temporaries holding flow 
C                              transport
C                              o uTrans: Zonal transport
C                              o vTrans: Meridional transport
C                              o rTrans: Vertical transport
C     maskUp                   o maskUp: land/water mask for W points
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
      _RS maskUp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerTr1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL phiHyd  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL rhokm1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhok    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KappaRT (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRS (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL sigmaX  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaY  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaR  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
C     This is currently used by IVDC and Diagnostics
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
CEOP
 
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
         sigmaX(i,j,k) = 0. _d 0
         sigmaY(i,j,k) = 0. _d 0
         sigmaR(i,j,k) = 0. _d 0
        ENDDO
        rhoKM1 (i,j) = 0. _d 0
        rhok   (i,j) = 0. _d 0
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
CHPF$  INDEPENDENT, NEW (rTrans,fVerT,fVerS
CHPF$&                  ,phiHyd,utrans,vtrans,xA,yA
CHPF$&                  ,KappaRT,KappaRS
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
          rTrans (i,j)   = 0. _d 0
          fVerT  (i,j,1) = 0. _d 0
          fVerT  (i,j,2) = 0. _d 0
          fVerS  (i,j,1) = 0. _d 0
          fVerS  (i,j,2) = 0. _d 0
          fVerTr1(i,j,1) = 0. _d 0
          fVerTr1(i,j,2) = 0. _d 0
         ENDDO
        ENDDO

        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
C This is currently also used by IVDC and Diagnostics
           ConvectCount(i,j,k) = 0.
           KappaRT(i,j,k) = 0. _d 0
           KappaRS(i,j,k) = 0. _d 0
#ifdef ALLOW_AUTODIFF_TAMC
           gT(i,j,k,bi,bj) = 0. _d 0
           gS(i,j,k,bi,bj) = 0. _d 0
#ifdef ALLOW_PASSIVE_TRACER
           gTr1(i,j,k,bi,bj) = 0. _d 0
#endif
#endif
          ENDDO
         ENDDO
        ENDDO

        iMin = 1-OLx+1
        iMax = sNx+OLx
        jMin = 1-OLy+1
        jMax = sNy+OLy


#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE salt (:,:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#ifdef ALLOW_KPP
CADJ STORE uvel (:,:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE vvel (:,:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Start of diagnostic loop
        DO k=Nr,1,-1

#ifdef ALLOW_AUTODIFF_TAMC
C? Patrick, is this formula correct now that we change the loop range?
C? Do we still need this?
cph kkey formula corrected. 
cph Needed for rhok, rhokm1, in the case useGMREDI.
         kkey = (ikey-1)*Nr + k
CADJ STORE rhokm1(:,:) = comlev1_bibj_k ,       key=kkey, byte=isbyte
CADJ STORE rhok  (:,:) = comlev1_bibj_k ,       key=kkey, byte=isbyte
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
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE salt (:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
            CALL FIND_RHO(
     I        bi, bj, iMin, iMax, jMin, jMax, k, k, eosType,
     I        theta, salt,
     O        rhoK,
     I        myThid )
            IF (k.GT.1) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,k-1,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE salt (:,:,k-1,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
             CALL FIND_RHO(
     I        bi, bj, iMin, iMax, jMin, jMax, k-1, k, eosType,
     I        theta, salt,
     O        rhoKm1,
     I        myThid )
            ENDIF
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

#ifdef ALLOW_AUTODIFF_TAMC
cph avoids recomputation of integrate_for_w
CADJ STORE wvel (:,:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

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
#ifdef ALLOW_AUTODIFF_TAMC
cph needed for KPP
CADJ STORE surfacetendencyU(:,:,bi,bj)
CADJ &     = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE surfacetendencyV(:,:,bi,bj)
CADJ &     = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE surfacetendencyS(:,:,bi,bj)
CADJ &     = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE surfacetendencyT(:,:,bi,bj)
CADJ &     = comlev1_bibj, key=ikey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef  ALLOW_GMREDI

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sigmaX(:,:,:) = comlev1, key=ikey, byte=isbyte
CADJ STORE sigmaY(:,:,:) = comlev1, key=ikey, byte=isbyte
CADJ STORE sigmaR(:,:,:) = comlev1, key=ikey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
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

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE Kwx(:,:,:,bi,bj)   = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE Kwy(:,:,:,bi,bj)   = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE Kwz(:,:,:,bi,bj)   = comlev1_bibj, key=ikey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#endif  /* ALLOW_GMREDI */

#ifdef  ALLOW_KPP
C--     Compute KPP mixing coefficients
        IF (useKPP) THEN
          CALL KPP_CALC(
     I                  bi, bj, myTime, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
        ELSE
          CALL KPP_CALC_DUMMY(
     I                  bi, bj, myTime, myThid )
#endif /* ALLOW_AUTODIFF_TAMC */
        ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KPPghat   (:,:,:,bi,bj)
CADJ &   , KPPdiffKzT(:,:,:,bi,bj)
CADJ &   , KPPdiffKzS(:,:,:,bi,bj)
CADJ &   , KPPfrac   (:,:  ,bi,bj)
CADJ &                 = comlev1_bibj, key=ikey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#endif  /* ALLOW_KPP */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KappaRT(:,:,:)     = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE KappaRS(:,:,:)     = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE theta(:,:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE salt (:,:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE uvel (:,:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE vvel (:,:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#ifdef ALLOW_PASSIVE_TRACER
CADJ STORE tr1  (:,:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AIM
C       AIM - atmospheric intermediate model, physics package code.
C note(jmc) : phiHyd=0 at this point but is not really used in Molteni Physics
        IF ( useAIM ) THEN
         CALL TIMER_START('AIM_DO_ATMOS_PHYS      [DYNAMICS]', myThid)
         CALL AIM_DO_ATMOS_PHYSICS( phiHyd, bi, bj, myTime, myThid )
         CALL TIMER_STOP ('AIM_DO_ATMOS_PHYS      [DYNAMICS]', myThid)
        ENDIF
#endif /* ALLOW_AIM */

#ifndef DISABLE_MULTIDIM_ADVECTION
C--     Some advection schemes are better calculated using a multi-dimensional
C       method in the absence of any other terms and, if used, is done here.
C
C The CPP flag DISABLE_MULTIDIM_ADVECTION is currently unset in GAD_OPTIONS.h
C The default is to use multi-dimensinal advection for non-linear advection
C schemes. However, for the sake of efficiency of the adjoint it is necessary
C to be able to exclude this scheme to avoid excessive storage and
C recomputation. It *is* differentiable, if you need it.
C Edit GAD_OPTIONS.h and #define DISABLE_MULTIDIM_ADVECTION to
C disable this section of code.
        IF (multiDimAdvection) THEN
         IF (tempStepping .AND.
     &       tempAdvScheme.NE.ENUM_CENTERED_2ND .AND.
     &       tempAdvScheme.NE.ENUM_UPWIND_3RD .AND.
     &       tempAdvScheme.NE.ENUM_CENTERED_4TH ) THEN
          CALL GAD_ADVECTION(bi,bj,tempAdvScheme,GAD_TEMPERATURE,
     U                      theta,gT,
     I                      myTime,myIter,myThid)
         ENDIF
         IF (saltStepping .AND.
     &       saltAdvScheme.NE.ENUM_CENTERED_2ND .AND.
     &       saltAdvScheme.NE.ENUM_UPWIND_3RD .AND.
     &       saltAdvScheme.NE.ENUM_CENTERED_4TH ) THEN
          CALL GAD_ADVECTION(bi,bj,saltAdvScheme,GAD_SALINITY,
     U                      salt,gS,
     I                      myTime,myIter,myThid)
         ENDIF
        ENDIF
#endif /* DISABLE_MULTIDIM_ADVECTION */

C--     Start of thermodynamics loop
        DO k=Nr,1,-1
#ifdef ALLOW_AUTODIFF_TAMC 
C? Patrick Is this formula correct?
cph Yes, but I rewrote it.
cph Also, the KappaR? need the index and subscript k!
         kkey = (ikey-1)*Nr + k
#endif /* ALLOW_AUTODIFF_TAMC */

C--       km1    Points to level above k (=k-1)
C--       kup    Cycles through 1,2 to point to layer above
C--       kDown  Cycles through 2,1 to point to current layer

          km1  = MAX(1,k-1)
          kup  = 1+MOD(k+1,2)
          kDown= 1+MOD(k,2)

          iMin = 1-OLx
          iMax = sNx+OLx
          jMin = 1-OLy
          jMax = sNy+OLy

C--       Get temporary terms used by tendency routines
          CALL CALC_COMMON_FACTORS (
     I         bi,bj,iMin,iMax,jMin,jMax,k,
     O         xA,yA,uTrans,vTrans,rTrans,maskUp,
     I         myThid)

#ifdef ALLOW_AUTODIFF_TAMC 
CADJ STORE KappaRT(:,:,k)    = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE KappaRS(:,:,k)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef  INCLUDE_CALC_DIFFUSIVITY_CALL
C--      Calculate the total vertical diffusivity
         CALL CALC_DIFFUSIVITY(
     I        bi,bj,iMin,iMax,jMin,jMax,k,
     I        maskUp,
     O        KappaRT,KappaRS,
     I        myThid)
#endif

          iMin = 1-OLx+2
          iMax = sNx+OLx-1
          jMin = 1-OLy+2
          jMax = sNy+OLy-1

C--      Calculate active tracer tendencies (gT,gS,...)
C        and step forward storing result in gTnm1, gSnm1, etc.
         IF ( tempStepping ) THEN
           CALL CALC_GT(
     I         bi,bj,iMin,iMax,jMin,jMax, k,km1,kup,kDown,
     I         xA,yA,uTrans,vTrans,rTrans,maskUp,
     I         KappaRT,
     U         fVerT,
     I         myTime,myIter,myThid)
           CALL TIMESTEP_TRACER(
     I         bi,bj,iMin,iMax,jMin,jMax,k,tempAdvScheme,
     I         theta, gT,
     I         myIter, myThid)
         ENDIF
         IF ( saltStepping ) THEN
           CALL CALC_GS(
     I         bi,bj,iMin,iMax,jMin,jMax, k,km1,kup,kDown,
     I         xA,yA,uTrans,vTrans,rTrans,maskUp,
     I         KappaRS,
     U         fVerS,
     I         myTime,myIter,myThid)
           CALL TIMESTEP_TRACER(
     I         bi,bj,iMin,iMax,jMin,jMax,k,saltAdvScheme,
     I         salt, gS,
     I         myIter, myThid)
         ENDIF
#ifdef ALLOW_PASSIVE_TRACER
         IF ( tr1Stepping ) THEN
           CALL CALC_GTR1(
     I         bi,bj,iMin,iMax,jMin,jMax, k,km1,kup,kDown,
     I         xA,yA,uTrans,vTrans,rTrans,maskUp,
     I         KappaRT,
     U         fVerTr1,
     I         myTime,myIter,myThid)
           CALL TIMESTEP_TRACER(
     I         bi,bj,iMin,iMax,jMin,jMax,k,tracerAdvScheme,
     I         Tr1, gTr1,
     I         myIter,myThid)
         ENDIF
#endif

#ifdef   ALLOW_OBCS
C--      Apply open boundary conditions
         IF (useOBCS) THEN
           CALL OBCS_APPLY_TS( bi, bj, k, gT, gS, myThid )
         END IF
#endif   /* ALLOW_OBCS */

C--      Freeze water
         IF (allowFreezing) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE gT(:,:,k,bi,bj) = comlev1_bibj_k
CADJ &   , key = kkey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
            CALL FREEZE( bi, bj, iMin, iMax, jMin, jMax, k, myThid )
         END IF

C--     end of thermodynamic k loop (Nr:1)
        ENDDO


#ifdef ALLOW_AUTODIFF_TAMC
C? Patrick? What about this one?
cph Keys iikey and idkey dont seem to be needed
cph since storing occurs on different tape for each
cph impldiff call anyways.
cph Thus, common block comlev1_impl isnt needed either.
cph Storing below needed in the case useGMREDI.
        iikey = (ikey-1)*maximpl
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Implicit diffusion
        IF (implicitDiffusion) THEN

         IF (tempStepping) THEN
#ifdef ALLOW_AUTODIFF_TAMC
            idkey = iikey + 1
CADJ STORE gT(:,:,:,bi,bj) = comlev1_bibj , key=ikey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
            CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTtracer, KappaRT, recip_HFacC,
     U         gT,
     I         myThid )
         ENDIF

         IF (saltStepping) THEN
#ifdef ALLOW_AUTODIFF_TAMC
         idkey = iikey + 2
CADJ STORE gS(:,:,:,bi,bj) = comlev1_bibj , key=ikey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
            CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTtracer, KappaRS, recip_HFacC,
     U         gS,
     I         myThid )
         ENDIF

#ifdef ALLOW_PASSIVE_TRACER
         IF (tr1Stepping) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE gTr1(:,:,:,bi,bj) = comlev1_bibj , key=ikey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I      bi, bj, iMin, iMax, jMin, jMax,
     I      deltaTtracer, KappaRT, recip_HFacC,
     U      gTr1,
     I      myThid )
         ENDIF
#endif

#ifdef   ALLOW_OBCS
C--      Apply open boundary conditions
         IF (useOBCS) THEN
           DO K=1,Nr
             CALL OBCS_APPLY_TS( bi, bj, k, gT, gS, myThid )
           ENDDO
         END IF
#endif   /* ALLOW_OBCS */

C--     End If implicitDiffusion
        ENDIF

Ccs-
       ENDDO
      ENDDO

#ifdef ALLOW_AIM
      IF ( useAIM ) THEN
       CALL AIM_AIM2DYN_EXCHANGES( myTime, myThid )
      ENDIF
       _EXCH_XYZ_R8(gT,myThid)
       _EXCH_XYZ_R8(gS,myThid)
#else
      IF (staggerTimeStep.AND.useCubedSphereExchange) THEN
       _EXCH_XYZ_R8(gT,myThid)
       _EXCH_XYZ_R8(gS,myThid)
      ENDIF
#endif /* ALLOW_AIM */

      RETURN
      END
