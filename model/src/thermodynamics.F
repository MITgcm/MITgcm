C $Header: /u/gcmpack/MITgcm/model/src/thermodynamics.F,v 1.55 2003/10/26 01:10:12 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_GMREDI
#  include "GMREDI_OPTIONS.h"
# endif
# ifdef ALLOW_KPP
#  include "KPP_OPTIONS.h"
# endif
#ifdef ALLOW_PTRACERS
# include "PTRACERS_OPTIONS.h"
#endif
#endif /* ALLOW_AUTODIFF_TAMC */

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
#ifdef ALLOW_PTRACERS
#include "PTRACERS.h"
#endif
#ifdef ALLOW_TIMEAVE
#include "TIMEAVE_STATV.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
# include "FFIELDS.h"
# include "EOS.h"
# ifdef ALLOW_KPP
#  include "KPP.h"
# endif
# ifdef ALLOW_GMREDI
#  include "GMREDI.h"
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

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
C     phiSurfX, - gradient of Surface potentiel (Pressure/rho, ocean)
C     phiSurfY             or geopotentiel (atmos) in X and Y direction
C     KappaRT,       - Total diffusion in vertical for T and S.
C     KappaRS          (background + spatially varying, isopycnal term).
C     useVariableK   = T when vertical diffusion is not constant
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
#ifdef ALLOW_PASSIVE_TRACER
      _RL fVerTr1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
#endif
#ifdef ALLOW_PTRACERS
      _RL fVerP   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2,PTRACERS_num)
#endif
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
      LOGICAL useVariableK
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER bi, bj
      INTEGER i, j
      INTEGER k, km1, kup, kDown
      INTEGER iTracer, ip

CEOP

#ifndef DISABLE_DEBUGMODE
         IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_ENTER('FORWARD_STEP',myThid)
#endif
 
#ifdef ALLOW_AUTODIFF_TAMC
C--   dummy statement to end declaration part
      ikey = 1
      itdkey = 1
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AUTODIFF_TAMC
C--   HPF directive to help TAMC
CHPF$ INDEPENDENT
#endif /* ALLOW_AUTODIFF_TAMC */

      DO bj=myByLo(myThid),myByHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
C--    HPF directive to help TAMC
CHPF$  INDEPENDENT, NEW (rTrans,fVerT,fVerS
CHPF$&                  ,utrans,vtrans,xA,yA
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
          itdkey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Set up work arrays with valid (i.e. not NaN) values
C     These inital values do not alter the numerical results. They
C     just ensure that all memory references are to valid floating
C     point numbers. This prevents spurious hardware signals due to
C     uninitialised but inert locations.

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          xA(i,j)        = 0. _d 0
          yA(i,j)        = 0. _d 0
          uTrans(i,j)    = 0. _d 0
          vTrans(i,j)    = 0. _d 0
          rhok   (i,j)   = 0. _d 0
          rhoKM1 (i,j)   = 0. _d 0
          phiSurfX(i,j)  = 0. _d 0
          phiSurfY(i,j)  = 0. _d 0
          rTrans (i,j)   = 0. _d 0
          fVerT  (i,j,1) = 0. _d 0
          fVerT  (i,j,2) = 0. _d 0
          fVerS  (i,j,1) = 0. _d 0
          fVerS  (i,j,2) = 0. _d 0
#ifdef ALLOW_PASSIVE_TRACER
          fVerTr1(i,j,1) = 0. _d 0
          fVerTr1(i,j,2) = 0. _d 0
#endif
#ifdef ALLOW_PTRACERS
          DO ip=1,PTRACERS_num
           fVerP  (i,j,1,ip) = 0. _d 0
           fVerP  (i,j,2,ip) = 0. _d 0
          ENDDO
#endif
         ENDDO
        ENDDO

        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
C This is currently also used by IVDC and Diagnostics
           sigmaX(i,j,k) = 0. _d 0
           sigmaY(i,j,k) = 0. _d 0
           sigmaR(i,j,k) = 0. _d 0
           ConvectCount(i,j,k) = 0.
           KappaRT(i,j,k)    = 0. _d 0
           KappaRS(i,j,k)    = 0. _d 0
C- tracer tendency needs to be set to zero (moved here from gad_calc_rhs):
           gT(i,j,k,bi,bj)   = 0. _d 0
           gS(i,j,k,bi,bj)   = 0. _d 0
# ifdef ALLOW_PASSIVE_TRACER
ceh3 needs an IF ( use PASSIVE_TRACER) THEN
           gTr1(i,j,k,bi,bj) = 0. _d 0
# endif
# ifdef ALLOW_PTRACERS
ceh3 this should have an   IF ( usePTRACERS ) THEN
           DO iTracer=1,PTRACERS_numInUse
            gPTr(i,j,k,bi,bj,itracer) = 0. _d 0
           ENDDO
# endif
#ifdef ALLOW_AUTODIFF_TAMC
cph all the following init. are necessary for TAF
cph although some of these are re-initialised later.
# ifdef ALLOW_GMREDI
           Kwx(i,j,k,bi,bj)  = 0. _d 0
           Kwy(i,j,k,bi,bj)  = 0. _d 0
           Kwz(i,j,k,bi,bj)  = 0. _d 0
#  ifdef GM_NON_UNITY_DIAGONAL
           Kux(i,j,k,bi,bj)  = 0. _d 0
           Kvy(i,j,k,bi,bj)  = 0. _d 0
#  endif
#  ifdef GM_EXTRA_DIAGONAL
           Kuz(i,j,k,bi,bj)  = 0. _d 0
           Kvz(i,j,k,bi,bj)  = 0. _d 0
#  endif
#  ifdef GM_BOLUS_ADVEC
           GM_PsiX(i,j,k,bi,bj)  = 0. _d 0
           GM_PsiY(i,j,k,bi,bj)  = 0. _d 0
#  endif
#  ifdef GM_VISBECK_VARIABLE_K
           VisbeckK(i,j,bi,bj)   = 0. _d 0
#  endif
# endif /* ALLOW_GMREDI */
#endif /* ALLOW_AUTODIFF_TAMC */
          ENDDO
         ENDDO
        ENDDO

        iMin = 1-OLx
        iMax = sNx+OLx
        jMin = 1-OLy
        jMax = sNy+OLy

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE salt (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE totphihyd
CADJ &     = comlev1_bibj, key=itdkey, byte=isbyte
#ifdef ALLOW_KPP
CADJ STORE uvel (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE vvel (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
#endif
#endif /* ALLOW_AUTODIFF_TAMC */

#ifndef DISABLE_DEBUGMODE
        IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_MSG('ENTERING UPWARD K LOOP',myThid)
#endif

C--     Start of diagnostic loop
        DO k=Nr,1,-1

#ifdef ALLOW_AUTODIFF_TAMC
C? Patrick, is this formula correct now that we change the loop range?
C? Do we still need this?
cph kkey formula corrected. 
cph Needed for rhok, rhokm1, in the case useGMREDI.
         kkey = (itdkey-1)*Nr + k
#endif /* ALLOW_AUTODIFF_TAMC */

C--       Integrate continuity vertically for vertical velocity
c         CALL INTEGRATE_FOR_W(
c    I                         bi, bj, k, uVel, vVel,
c    O                         wVel,
c    I                         myThid )

#ifdef    ALLOW_OBCS
#ifdef    ALLOW_NONHYDROSTATIC
C--       Apply OBC to W if in N-H mode
c         IF (useOBCS.AND.nonHydrostatic) THEN
c           CALL OBCS_APPLY_W( bi, bj, k, wVel, myThid )
c         ENDIF
#endif    /* ALLOW_NONHYDROSTATIC */
#endif    /* ALLOW_OBCS */

C--       Attention: by defining "SINGLE_LAYER_MODE" in CPP_OPTIONS.h 
C--       MOST of THERMODYNAMICS will be disabled
#ifndef SINGLE_LAYER_MODE

C--       Calculate gradients of potential density for isoneutral
C         slope terms (e.g. GM/Redi tensor or IVDC diffusivity)
c         IF ( k.GT.1 .AND. (useGMRedi.OR.ivdc_kappa.NE.0.) ) THEN
          IF ( useGMRedi .OR. (k.GT.1 .AND. ivdc_kappa.NE.0.) ) THEN
#ifndef DISABLE_DEBUGMODE
            IF ( debugLevel .GE. debLevB ) 
     &       CALL DEBUG_CALL('FIND_RHO',myThid)
#endif
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE salt (:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
            CALL FIND_RHO(
     I        bi, bj, iMin, iMax, jMin, jMax, k, k,
     I        theta, salt,
     O        rhoK,
     I        myThid )

            IF (k.GT.1) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,k-1,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE salt (:,:,k-1,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
             CALL FIND_RHO(
     I        bi, bj, iMin, iMax, jMin, jMax, k-1, k,
     I        theta, salt,
     O        rhoKm1,
     I        myThid )
            ENDIF
#ifndef DISABLE_DEBUGMODE
            IF ( debugLevel .GE. debLevB ) 
     &       CALL DEBUG_CALL('GRAD_SIGMA',myThid)
#endif
            CALL GRAD_SIGMA(
     I             bi, bj, iMin, iMax, jMin, jMax, k,
     I             rhoK, rhoKm1, rhoK,
     O             sigmaX, sigmaY, sigmaR,
     I             myThid )
          ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rhok   (:,:) = comlev1_bibj_k ,       key=kkey, byte=isbyte
CADJ STORE rhokm1 (:,:) = comlev1_bibj_k ,       key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C--       Implicit Vertical Diffusion for Convection
c ==> should use sigmaR !!!
          IF (k.GT.1 .AND. ivdc_kappa.NE.0.) THEN
#ifndef DISABLE_DEBUGMODE
            IF ( debugLevel .GE. debLevB ) 
     &       CALL DEBUG_CALL('CALC_IVDC',myThid)
#endif
            CALL CALC_IVDC(
     I        bi, bj, iMin, iMax, jMin, jMax, k,
     I        rhoKm1, rhoK,
     U        ConvectCount, KappaRT, KappaRS,
     I        myTime, myIter, myThid)
          ENDIF

#endif /* SINGLE_LAYER_MODE */

C--     end of diagnostic k loop (Nr:1)
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
cph avoids recomputation of integrate_for_w
CADJ STORE wvel (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef  ALLOW_OBCS
C--     Calculate future values on open boundaries
        IF (useOBCS) THEN
#ifndef DISABLE_DEBUGMODE
          IF ( debugLevel .GE. debLevB ) 
     &     CALL DEBUG_CALL('OBCS_CALC',myThid)
#endif
          CALL OBCS_CALC( bi, bj, myTime+deltaT, myIter+1,
     I            uVel, vVel, wVel, theta, salt,
     I            myThid )
        ENDIF
#endif  /* ALLOW_OBCS */


#ifdef ALLOW_THERM_SEAICE
       IF (useThermSeaIce) THEN
#ifndef DISABLE_DEBUGMODE
        IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('ICE_FORCING',myThid)
#endif
C--     Determines forcing terms based on external fields
C       including effects from ice
        CALL ICE_FORCING(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             myThid )
       ELSE
#endif /* ALLOW_THERM_SEAICE */

C--     Determines forcing terms based on external fields
C       relaxation terms, etc.
#ifndef DISABLE_DEBUGMODE
        IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_CALL('EXTERNAL_FORCING_SURF',myThid)
#endif
        CALL EXTERNAL_FORCING_SURF(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             myTime, myIter, myThid )

#ifdef ALLOW_THERM_SEAICE
C--    end of if/else block useThermSeaIce --
       ENDIF
#endif /* ALLOW_THERM_SEAICE */

#ifdef ALLOW_AUTODIFF_TAMC
cph needed for KPP
CADJ STORE surfacetendencyU(:,:,bi,bj)
CADJ &     = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE surfacetendencyV(:,:,bi,bj)
CADJ &     = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE surfacetendencyS(:,:,bi,bj)
CADJ &     = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE surfacetendencyT(:,:,bi,bj)
CADJ &     = comlev1_bibj, key=itdkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Attention: by defining "SINGLE_LAYER_MODE" in CPP_OPTIONS.h 
C--     MOST of THERMODYNAMICS will be disabled
#ifndef SINGLE_LAYER_MODE

#ifdef  ALLOW_GMREDI

#ifdef ALLOW_AUTODIFF_TAMC
cph storing here is needed only for one GMREDI_OPTIONS:
cph define GM_BOLUS_ADVEC
cph but I've avoided the #ifdef for now, in case more things change
CADJ STORE sigmaX(:,:,:)        = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE sigmaY(:,:,:)        = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE sigmaR(:,:,:)        = comlev1_bibj, key=itdkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Calculate iso-neutral slopes for the GM/Redi parameterisation
        IF (useGMRedi) THEN
#ifndef DISABLE_DEBUGMODE
          IF ( debugLevel .GE. debLevB ) 
     &     CALL DEBUG_CALL('GMREDI_CALC_TENSOR',myThid)
#endif
          CALL GMREDI_CALC_TENSOR(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR,
     I             myThid )
#ifdef ALLOW_AUTODIFF_TAMC
        ELSE
          CALL GMREDI_CALC_TENSOR_DUMMY(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR,
     I             myThid )
#endif /* ALLOW_AUTODIFF_TAMC */
        ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE Kwx(:,:,:,bi,bj)   = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE Kwy(:,:,:,bi,bj)   = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE Kwz(:,:,:,bi,bj)   = comlev1_bibj, key=itdkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#endif  /* ALLOW_GMREDI */

#ifdef  ALLOW_KPP
C--     Compute KPP mixing coefficients
        IF (useKPP) THEN
#ifndef DISABLE_DEBUGMODE
          IF ( debugLevel .GE. debLevB ) 
     &     CALL DEBUG_CALL('KPP_CALC',myThid)
#endif
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
CADJ &                 = comlev1_bibj, key=itdkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#endif  /* ALLOW_KPP */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE salt (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE uvel (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE vvel (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
#ifdef ALLOW_PASSIVE_TRACER
CADJ STORE tr1  (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
#endif
#ifdef ALLOW_PTRACERS
cph-- moved to forward_step to avoid key computation
cphCADJ STORE ptracer(:,:,:,bi,bj,itracer) = comlev1_bibj,
cphCADJ &                              key=itdkey, byte=isbyte
#endif
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_AIM
C       AIM - atmospheric intermediate model, physics package code.
        IF ( useAIM ) THEN
#ifndef DISABLE_DEBUGMODE
          IF ( debugLevel .GE. debLevB ) 
     &     CALL DEBUG_CALL('AIM_DO_PHYSICS',myThid)
#endif
         CALL TIMER_START('AIM_DO_PHYSICS   [THERMODYNAMICS]', myThid)
         CALL AIM_DO_PHYSICS( bi, bj, myTime, myIter, myThid )
         CALL TIMER_STOP( 'AIM_DO_PHYSICS   [THERMODYNAMICS]', myThid)
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
        IF (tempMultiDimAdvec) THEN
#ifndef DISABLE_DEBUGMODE
          IF ( debugLevel .GE. debLevB ) 
     &     CALL DEBUG_CALL('GAD_ADVECTION',myThid)
#endif
          CALL GAD_ADVECTION(bi,bj,tempAdvScheme,GAD_TEMPERATURE,
     U                      theta,gT,
     I                      myTime,myIter,myThid)
        ENDIF
        IF (saltMultiDimAdvec) THEN
#ifndef DISABLE_DEBUGMODE
          IF ( debugLevel .GE. debLevB ) 
     &     CALL DEBUG_CALL('GAD_ADVECTION',myThid)
#endif
          CALL GAD_ADVECTION(bi,bj,saltAdvScheme,GAD_SALINITY,
     U                      salt,gS,
     I                      myTime,myIter,myThid)
        ENDIF
C Since passive tracers are configurable separately from T,S we
C call the multi-dimensional method for PTRACERS regardless
C of whether multiDimAdvection is set or not.
#ifdef ALLOW_PTRACERS
        IF ( usePTRACERS ) THEN
#ifndef DISABLE_DEBUGMODE
          IF ( debugLevel .GE. debLevB ) 
     &     CALL DEBUG_CALL('PTRACERS_ADVECTION',myThid)
#endif
         CALL PTRACERS_ADVECTION( bi,bj,myIter,myTime,myThid )
        ENDIF
#endif /* ALLOW_PTRACERS */
#endif /* DISABLE_MULTIDIM_ADVECTION */

#ifndef DISABLE_DEBUGMODE
       IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_MSG('ENTERING DOWNWARD K LOOP',myThid)
#endif

C--     Start of thermodynamics loop
        DO k=Nr,1,-1
#ifdef ALLOW_AUTODIFF_TAMC 
C? Patrick Is this formula correct?
cph Yes, but I rewrote it.
cph Also, the KappaR? need the index and subscript k!
         kkey = (itdkey-1)*Nr + k
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

#ifdef ALLOW_GMREDI

C--   Residual transp = Bolus transp + Eulerian transp
          IF (useGMRedi) THEN
            CALL GMREDI_CALC_UVFLOW(
     &            uTrans, vTrans, bi, bj, k, myThid) 
            IF (K.GE.2) CALL GMREDI_CALC_WFLOW(
     &                    rTrans, bi, bj, k, myThid) 
          ENDIF

#ifdef ALLOW_AUTODIFF_TAMC 
#ifdef GM_BOLUS_ADVEC
CADJ STORE uTrans(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE vTrans(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE rTrans(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
#endif /* ALLOW_AUTODIFF_TAMC */

#endif /* ALLOW_GMREDI */

#ifdef  INCLUDE_CALC_DIFFUSIVITY_CALL
C--      Calculate the total vertical diffusivity
         CALL CALC_DIFFUSIVITY(
     I        bi,bj,iMin,iMax,jMin,jMax,k,
     I        maskUp,
     O        KappaRT,KappaRS,
     I        myThid)
# ifdef ALLOW_AUTODIFF_TAMC 
CADJ STORE KappaRT(:,:,k)    = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE KappaRS(:,:,k)    = comlev1_bibj_k, key=kkey, byte=isbyte
# endif /* ALLOW_AUTODIFF_TAMC */
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

#ifdef ALLOW_THERM_SEAICE
         IF (useThermSeaIce .AND. k.EQ.1) THEN
           CALL ICE_FREEZE( bi,bj, iMin,iMax,jMin,jMax, myThid )
         ENDIF
#endif

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
ceh3 needs an IF ( usePASSIVE_TRACER ) THEN
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
#ifdef ALLOW_PTRACERS
         IF ( usePTRACERS ) THEN
           CALL PTRACERS_INTEGRATE(
     I         bi,bj,k,
     I         xA,yA,uTrans,vTrans,rTrans,maskUp,
     X         fVerP, KappaRS,
     I         myIter,myTime,myThid)
         ENDIF
#endif /* ALLOW_PTRACERS */

#ifdef   ALLOW_OBCS
C--      Apply open boundary conditions
         IF (useOBCS) THEN
           CALL OBCS_APPLY_TS( bi, bj, k, gT, gS, myThid )
         END IF
#endif   /* ALLOW_OBCS */

C--      Freeze water
         IF ( allowFreezing .AND. .NOT. useSEAICE
     &       .AND. .NOT.(useThermSeaIce.AND.k.EQ.1) ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE gT(:,:,k,bi,bj) = comlev1_bibj_k
CADJ &   , key = kkey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
            CALL FREEZE( bi, bj, iMin, iMax, jMin, jMax, k, myThid )
         ENDIF

C--     end of thermodynamic k loop (Nr:1)
        ENDDO

cswdice -- add ---
#ifdef ALLOW_THERM_SEAICE
c timeaveraging for ice model values
ceh3 This should be wrapped in an IF ( useThermSeaIce ) THEN
           CALL ICE_AVE(bi,bj,iMin,iMax,jMin,jMax,myThid )
#endif
cswdice --- end add ---




C--     Implicit diffusion
        IF (implicitDiffusion) THEN

         IF (tempStepping) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KappaRT(:,:,:) = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE gT(:,:,:,bi,bj) = comlev1_bibj , key=itdkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
            CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTtracer, KappaRT, recip_HFacC,
     U         gT,
     I         myThid )
         ENDIF

         IF (saltStepping) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE KappaRS(:,:,:) = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE gS(:,:,:,bi,bj) = comlev1_bibj , key=itdkey, byte=isbyte
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
CADJ STORE gTr1(:,:,:,bi,bj) = comlev1_bibj , key=itdkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I      bi, bj, iMin, iMax, jMin, jMax,
     I      deltaTtracer, KappaRT, recip_HFacC,
     U      gTr1,
     I      myThid )
         ENDIF
#endif

#ifdef ALLOW_PTRACERS
C Vertical diffusion (implicit) for passive tracers
         IF ( usePTRACERS ) THEN
           CALL PTRACERS_IMPLDIFF( bi,bj,KappaRS,myThid )
         ENDIF
#endif /* ALLOW_PTRACERS */

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

#ifdef ALLOW_TIMEAVE
ceh3 needs an IF ( useTIMEAVE ) THEN
        IF (taveFreq.GT.0. .AND. ivdc_kappa.NE.0.) THEN
          CALL TIMEAVE_CUMUL_1T(ConvectCountTave, ConvectCount,
     I                           Nr, deltaTclock, bi, bj, myThid)
        ENDIF
        useVariableK = useKPP .OR. useGMredi .OR. ivdc_kappa.NE.0.
        IF (taveFreq.GT.0. .AND. useVariableK ) THEN
         IF (implicitDiffusion) THEN
          CALL TIMEAVE_CUMUL_DIF_1T(TdiffRtave, gT, kappaRT,
     I                        Nr, 3, deltaTclock, bi, bj, myThid)
         ELSE
          CALL TIMEAVE_CUMUL_DIF_1T(TdiffRtave, theta, kappaRT,
     I                        Nr, 3, deltaTclock, bi, bj, myThid)
         ENDIF
        ENDIF
#endif /* ALLOW_TIMEAVE */ 

#endif /* SINGLE_LAYER_MODE */

C--   end bi,bj loops.
       ENDDO
      ENDDO

#ifndef DISABLE_DEBUGMODE
      If (debugMode) THEN
       CALL DEBUG_STATS_RL(Nr,uVel,'Uvel (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,vVel,'Vvel (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,wVel,'Wvel (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,theta,'Theta (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,salt,'Salt (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,Gt,'Gt (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,Gs,'Gs (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,GtNm1,'GtNm1 (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,GsNm1,'GsNm1 (THERMODYNAMICS)',myThid)
#ifdef ALLOW_PTRACERS
       IF ( usePTRACERS ) THEN
         CALL PTRACERS_DEBUG(myThid)
       ENDIF
#endif /* ALLOW_PTRACERS */
      ENDIF
#endif

#ifndef DISABLE_DEBUGMODE
         IF ( debugLevel .GE. debLevB ) 
     &    CALL DEBUG_LEAVE('FORWARD_STEP',myThid)
#endif

      RETURN
      END
