C $Header: /u/gcmpack/MITgcm/model/src/thermodynamics.F,v 1.137 2011/06/08 01:21:14 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD_OPTIONS.h"
#endif
#ifdef ALLOW_LONGSTEP
#include "LONGSTEP_OPTIONS.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_GMREDI
#  include "GMREDI_OPTIONS.h"
# endif
# ifdef ALLOW_KPP
#  include "KPP_OPTIONS.h"
# endif
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
#include "RESTART.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
# include "GAD_SOM_VARS.h"
#endif
#ifdef ALLOW_PTRACERS
#ifndef ALLOW_LONGSTEP
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
# include "PTRACERS_FIELDS.h"
#endif
#endif
#ifdef ALLOW_TIMEAVE
# include "TIMEAVE_STATV.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
# include "FFIELDS.h"
# include "SURFACE.h"
# include "EOS.h"
# ifdef ALLOW_KPP
#  include "KPP.h"
# endif
# ifdef ALLOW_GMREDI
#  include "GMREDI.h"
# endif
# ifdef ALLOW_EBM
#  include "EBM.h"
# endif
# ifdef ALLOW_SALT_PLUME
#  include "SALT_PLUME.h"
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

#ifdef ALLOW_GENERIC_ADVDIFF
C     !LOCAL VARIABLES:
C     == Local variables
C     xA, yA                 - Per block temporaries holding face areas
C     uFld, vFld, wFld       - Local copy of velocity field (3 components)
C     uTrans, vTrans, rTrans - Per block temporaries holding flow transport
C                              o uTrans: Zonal transport
C                              o vTrans: Meridional transport
C                              o rTrans: Vertical transport
C     rTransKp1                o vertical volume transp. at interface k+1
C     maskUp                   o maskUp: land/water mask for W points
C     fVer[STUV]               o fVer: Vertical flux term - note fVer
C                                      is "pipelined" in the vertical
C                                      so we need an fVer for each
C                                      variable.
C     kappaRT,       - Total diffusion in vertical at level k, for T and S
C     kappaRS          (background + spatially varying, isopycnal term).
C     kappaRTr       - Total diffusion in vertical at level k,
C                      for each passive Tracer
C     kappaRk        - Total diffusion in vertical, all levels, 1 tracer
C     useVariableK   = T when vertical diffusion is not constant
C     iMin, iMax     - Ranges and sub-block indices on which calculations
C     jMin, jMax       are applied.
C     bi, bj
C     k, kup,        - Index for layer above and below. kup and kDown
C     kDown, km1       are switched with layer to be the appropriate
C                      index into fVerTerm.
      _RS xA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTrans  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTransKp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskUp  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fVerT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL fVerS   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL kappaRT (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL kappaRS (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
#ifdef ALLOW_PTRACERS
#ifndef ALLOW_LONGSTEP
      _RL fVerP   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,2,PTRACERS_num)
      _RL kappaRTr(1-Olx:sNx+Olx,1-Oly:sNy+Oly,PTRACERS_num)
#endif
#endif
      _RL kappaRk (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER bi, bj
      INTEGER i, j
      INTEGER k, km1, kup, kDown
#ifdef ALLOW_ADAMSBASHFORTH_3
      INTEGER iterNb, m1, m2
#endif
#ifdef ALLOW_TIMEAVE
      LOGICAL useVariableK
#endif
#ifdef ALLOW_PTRACERS
#ifndef ALLOW_LONGSTEP
      INTEGER iTracer, ip
#endif
#endif

CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('THERMODYNAMICS',myThid)
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

C-- Compute correction at the surface for Lin Free Surf.
#ifdef ALLOW_AUTODIFF_TAMC
      TsurfCor = 0. _d 0
      SsurfCor = 0. _d 0
#endif
      IF (linFSConserveTr) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta,salt,wvel = comlev1, key = ikey_dynamics, byte=isbyte
#endif
       CALL CALC_WSURF_TR(theta,salt,wVel,
     &                    myTime,myIter,myThid)
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
C--    HPF directive to help TAMC
CHPF$  INDEPENDENT, NEW (rTrans,fVerT,fVerS
CHPF$&                  ,utrans,vtrans,xA,yA
CHPF$&                  ,kappaRT,kappaRS
CHPF$&                  )
# ifdef ALLOW_PTRACERS
# ifndef ALLOW_LONGSTEP
CHPF$  INDEPENDENT, NEW (fVerP,kappaRTr)
# endif
# endif
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
          rTrans (i,j)   = 0. _d 0
          rTransKp1(i,j) = 0. _d 0
          fVerT  (i,j,1) = 0. _d 0
          fVerT  (i,j,2) = 0. _d 0
          fVerS  (i,j,1) = 0. _d 0
          fVerS  (i,j,2) = 0. _d 0
          kappaRT(i,j)   = 0. _d 0
          kappaRS(i,j)   = 0. _d 0
         ENDDO
        ENDDO

        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
C This is currently also used by IVDC and Diagnostics
           kappaRk(i,j,k)    = 0. _d 0
C- tracer tendency needs to be set to zero (moved here from gad_calc_rhs):
           gT(i,j,k,bi,bj)   = 0. _d 0
           gS(i,j,k,bi,bj)   = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

#ifdef ALLOW_PTRACERS
#ifndef ALLOW_LONGSTEP
        IF ( usePTRACERS ) THEN
         DO ip=1,PTRACERS_num
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             fVerP  (i,j,1,ip) = 0. _d 0
             fVerP  (i,j,2,ip) = 0. _d 0
             kappaRTr(i,j,ip)  = 0. _d 0
            ENDDO
           ENDDO
         ENDDO
C-      set tracer tendency to zero:
         DO iTracer=1,PTRACERS_num
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             gPTr(i,j,k,bi,bj,itracer) = 0. _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
#endif
#endif

#ifdef ALLOW_ADAMSBASHFORTH_3
C-      Apply AB on T,S :
        iterNb = myIter
        IF (staggerTimeStep) iterNb = myIter - 1
        m1 = 1 + MOD(iterNb+1,2)
        m2 = 1 + MOD( iterNb ,2)
C       compute T^n+1/2 (stored in gtNm) extrapolating T forward in time
        IF ( AdamsBashforth_T ) CALL ADAMS_BASHFORTH3(
     I                                  bi, bj, 0,
     U                                  theta, gtNm,
     I                                  tempStartAB, iterNb, myThid )
C       compute S^n+1/2 (stored in gsNm) extrapolating S forward in time
        IF ( AdamsBashforth_S ) CALL ADAMS_BASHFORTH3(
     I                                  bi, bj, 0,
     U                                  salt, gsNm,
     I                                  saltStartAB, iterNb, myThid )
#endif /* ALLOW_ADAMSBASHFORTH_3 */

c       iMin = 1-OLx
c       iMax = sNx+OLx
c       jMin = 1-OLy
c       jMax = sNy+OLy

#ifdef ALLOW_AUTODIFF_TAMC
cph avoids recomputation of integrate_for_w
CADJ STORE wvel (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Attention: by defining "SINGLE_LAYER_MODE" in CPP_OPTIONS.h
C--     MOST of THERMODYNAMICS will be disabled
#ifndef SINGLE_LAYER_MODE

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE salt (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE uvel (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE vvel (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
# if ((defined ALLOW_DEPTH_CONTROL) || (defined NONLIN_FRSURF))
#  ifndef ALLOW_ADAMSBASHFORTH_3
CADJ STORE gtnm1(:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE gsnm1(:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
#  endif
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

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
#ifdef GAD_ALLOW_TS_SOM_ADV
        IF ( tempSOM_Advection ) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('GAD_SOM_ADVECT',myThid)
#endif
          CALL GAD_SOM_ADVECT(
     I             tempImplVertAdv, tempAdvScheme, tempVertAdvScheme,
     I             GAD_TEMPERATURE, dTtracerLev,
     I             uVel, vVel, wVel, theta,
     U             som_T,
     O             gT,
     I             bi,bj,myTime,myIter,myThid)
        ELSEIF (tempMultiDimAdvec) THEN
#else /* GAD_ALLOW_TS_SOM_ADV */
        IF (tempMultiDimAdvec) THEN
#endif /* GAD_ALLOW_TS_SOM_ADV */
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('GAD_ADVECTION',myThid)
#endif
          CALL GAD_ADVECTION(
     I             tempImplVertAdv, tempAdvScheme, tempVertAdvScheme,
     I             GAD_TEMPERATURE, dTtracerLev,
     I             uVel, vVel, wVel, theta,
     O             gT,
     I             bi,bj,myTime,myIter,myThid)
        ENDIF
#ifdef GAD_ALLOW_TS_SOM_ADV
        IF ( saltSOM_Advection ) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('GAD_SOM_ADVECT',myThid)
#endif
          CALL GAD_SOM_ADVECT(
     I             saltImplVertAdv, saltAdvScheme, saltVertAdvScheme,
     I             GAD_SALINITY, dTtracerLev,
     I             uVel, vVel, wVel, salt,
     U             som_S,
     O             gS,
     I             bi,bj,myTime,myIter,myThid)
        ELSEIF (saltMultiDimAdvec) THEN
#else /* GAD_ALLOW_TS_SOM_ADV */
        IF (saltMultiDimAdvec) THEN
#endif /* GAD_ALLOW_TS_SOM_ADV */
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('GAD_ADVECTION',myThid)
#endif
          CALL GAD_ADVECTION(
     I             saltImplVertAdv, saltAdvScheme, saltVertAdvScheme,
     I             GAD_SALINITY, dTtracerLev,
     I             uVel, vVel, wVel, salt,
     O             gS,
     I             bi,bj,myTime,myIter,myThid)
        ENDIF

C Since passive tracers are configurable separately from T,S we
C call the multi-dimensional method for PTRACERS regardless
C of whether multiDimAdvection is set or not.
#ifdef ALLOW_PTRACERS
#ifndef ALLOW_LONGSTEP
        IF ( usePTRACERS ) THEN
#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('PTRACERS_ADVECTION',myThid)
#endif
         CALL PTRACERS_ADVECTION( bi,bj,myTime,myIter,myThid )
        ENDIF
#endif /* ALLOW_LONGSTEP */
#endif /* ALLOW_PTRACERS */
#endif /* DISABLE_MULTIDIM_ADVECTION */

#ifdef ALLOW_DEBUG
        IF (debugMode)
     &    CALL DEBUG_MSG('ENTERING DOWNWARD K LOOP',myThid)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef ALLOW_SALT_PLUME
CADJ STORE saltPlumeFlux(:,:,bi,bj)   =
CADJ &     comlev1_bibj, key=itdkey,kind = isbyte
CADJ STORE saltPlumeDepth(:,:,bi,bj)   =
CADJ &     comlev1_bibj, key=itdkey,kind = isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Start of thermodynamics loop
        DO k=Nr,1,-1
#ifdef ALLOW_AUTODIFF_TAMC
C? Patrick Is this formula correct?
cph Yes, but I rewrote it.
cph Also, the kappaR? need the index and subscript k!
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

          IF (k.EQ.Nr) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             rTransKp1(i,j) = 0. _d 0
            ENDDO
           ENDDO
          ELSE
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             rTransKp1(i,j) = rTrans(i,j)
            ENDDO
           ENDDO
          ENDIF
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rTransKp1(:,:) = comlev1_bibj_k, key=kkey, byte=isbyte
#endif

C--       Get temporary terms used by tendency routines :
C-        Calculate horizontal "volume transport" through tracer cell face
C         anelastic: uTrans,vTrans are scaled by rhoFacC (~ mass transport)
          CALL CALC_COMMON_FACTORS (
     I         uVel, vVel,
     O         uFld, vFld, uTrans, vTrans, xA, yA,
     I         k,bi,bj, myThid )

C-        Calculate vertical "volume transport" through tracer cell face
          IF (k.EQ.1) THEN
C-         Surface interface :
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             wFld(i,j)   = 0. _d 0
             maskUp(i,j) = 0. _d 0
             rTrans(i,j) = 0. _d 0
            ENDDO
           ENDDO
          ELSE
C-         Interior interface :
C          anelastic: rTrans is scaled by rhoFacF (~ mass transport)
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             wFld(i,j)   = wVel(i,j,k,bi,bj)
             maskUp(i,j) = maskC(i,j,k-1,bi,bj)*maskC(i,j,k,bi,bj)
             rTrans(i,j) = wFld(i,j)*rA(i,j,bi,bj)*maskUp(i,j)
     &                              *deepFac2F(k)*rhoFacF(k)
            ENDDO
           ENDDO
          ENDIF

#ifdef ALLOW_GMREDI
C--   Residual transp = Bolus transp + Eulerian transp
          IF (useGMRedi) THEN
            CALL GMREDI_CALC_UVFLOW(
     U                  uFld, vFld, uTrans, vTrans,
     I                  k, bi, bj, myThid )
            IF (K.GE.2) THEN
              CALL GMREDI_CALC_WFLOW(
     U                  wFld, rTrans,
     I                  k, bi, bj, myThid )
            ENDIF
          ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rTrans(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE wfld(:,:)      = comlev1_bibj_k, key=kkey, byte=isbyte
# ifdef GM_BOLUS_ADVEC
CADJ STORE ufld(:,:)      = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE vfld(:,:)      = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE uTrans(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE vTrans(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
# endif
# endif /* ALLOW_AUTODIFF_TAMC */
#endif /* ALLOW_GMREDI */

#ifdef  INCLUDE_CALC_DIFFUSIVITY_CALL
C--      Calculate the total vertical diffusivity
          IF ( .NOT.implicitDiffusion ) THEN
            CALL CALC_DIFFUSIVITY(
     I          bi,bj,iMin,iMax,jMin,jMax,k,
     I          maskUp,
     O          kappaRT,kappaRS,
     I          myThid)
          ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE kappaRT(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE kappaRS(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
# endif /* ALLOW_AUTODIFF_TAMC */
#endif

          iMin = 1-OLx+2
          iMax = sNx+OLx-1
          jMin = 1-OLy+2
          jMax = sNy+OLy-1

C--      Calculate active tracer tendencies (gT,gS,...)
C        and step forward storing result in gT, gS, etc.
C--
# ifdef ALLOW_AUTODIFF_TAMC
#  if ((defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)) && (defined ALLOW_GMREDI)
#   ifdef GM_NON_UNITY_DIAGONAL
CADJ STORE kux(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE kvy(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
#   endif
#   ifdef GM_EXTRA_DIAGONAL
CADJ STORE kuz(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE kvz(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
#   endif
#  endif
# endif /* ALLOW_AUTODIFF_TAMC */
C
#ifdef ALLOW_AUTODIFF_TAMC
# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
cph-test
CADJ STORE uFld(:,:), vFld(:,:), wFld(:,:)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE uTrans(:,:), vTrans(:,:)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE xA(:,:), yA(:,:)
CADJ &     = comlev1_bibj_k, key=kkey, byte=isbyte
#  ifdef ALLOW_ADAMSBASHFORTH_3
CADJ STORE gT(:,:,k,bi,bj)    = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE gS(:,:,k,bi,bj)    = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE gSnm(:,:,k,bi,bj,1)= comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE gSnm(:,:,k,bi,bj,2)= comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE gTnm(:,:,k,bi,bj,1)= comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE gTnm(:,:,k,bi,bj,2)= comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE theta(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE salt(:,:,k,bi,bj)  = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE fvert(:,:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE fvers(:,:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#  endif /* ALLOW_ADAMSBASHFORTH_3 */
# endif /* NONLIN_FRSURF */
#endif /* ALLOW_AUTODIFF_TAMC */
C
         IF ( tempStepping ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
# ifndef ALLOW_ADAMSBASHFORTH_3
CADJ STORE gTnm1(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
# else
#  ifndef NONLIN_FRSURF
CADJ STORE gTnm(:,:,k,bi,bj,1)= comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE gTnm(:,:,k,bi,bj,2)= comlev1_bibj_k, key=kkey, byte=isbyte
#  endif /* ndef NONLIN_FRSURF */
# endif /* ndef ALLOW_ADAMSBASHFORTH_3 */
# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
CADJ STORE gt(:,:,:,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE fvert(:,:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */
          CALL CALC_GT(
     I         bi,bj,iMin,iMax,jMin,jMax, k,km1,kup,kDown,
     I         xA, yA, maskUp, uFld, vFld, wFld,
     I         uTrans, vTrans, rTrans, rTransKp1,
     I         kappaRT,
     U         fVerT,
     I         myTime,myIter,myThid)
#ifdef ALLOW_ADAMSBASHFORTH_3
          IF ( AdamsBashforth_T ) THEN
           CALL TIMESTEP_TRACER(
     I         bi, bj, k, dTtracerLev(k),
     I         gtNm(1-Olx,1-Oly,1,1,1,m2),
     U         gT,
     I         myIter, myThid )
          ELSE
#endif
           CALL TIMESTEP_TRACER(
     I         bi, bj, k, dTtracerLev(k),
     I         theta,
     U         gT,
     I         myIter, myThid )
#ifdef ALLOW_ADAMSBASHFORTH_3
          ENDIF
#endif
         ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
# if (defined NONLIN_FRSURF) && (defined ALLOW_ADAMSBASHFORTH_3)
CADJ STORE gTnm(:,:,k,bi,bj,1)= comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE gTnm(:,:,k,bi,bj,2)= comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE fvert(:,:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
# endif
#endif

         IF ( saltStepping ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
# ifndef ALLOW_ADAMSBASHFORTH_3
CADJ STORE gSnm1(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
# else
#  ifndef NONLIN_FRSURF
CADJ STORE gSnm(:,:,k,bi,bj,1)= comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE gSnm(:,:,k,bi,bj,2)= comlev1_bibj_k, key=kkey, byte=isbyte
#  endif /* ndef NONLIN_FRSURF */
# endif /* ndef ALLOW_ADAMSBASHFORTH_3 */
# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
CADJ STORE gs(:,:,:,bi,bj)    = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE fvers(:,:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

          CALL CALC_GS(
     I         bi,bj,iMin,iMax,jMin,jMax, k,km1,kup,kDown,
     I         xA, yA, maskUp, uFld, vFld, wFld,
     I         uTrans, vTrans, rTrans, rTransKp1,
     I         kappaRS,
     U         fVerS,
     I         myTime,myIter,myThid)
#ifdef ALLOW_ADAMSBASHFORTH_3
          IF ( AdamsBashforth_S ) THEN
           CALL TIMESTEP_TRACER(
     I         bi, bj, k, dTtracerLev(k),
     I         gsNm(1-Olx,1-Oly,1,1,1,m2),
     U         gS,
     I         myIter, myThid )
          ELSE
#endif
           CALL TIMESTEP_TRACER(
     I         bi, bj, k, dTtracerLev(k),
     I         salt,
     U         gS,
     I         myIter, myThid )
#ifdef ALLOW_ADAMSBASHFORTH_3
          ENDIF
#endif
         ENDIF

#ifdef ALLOW_PTRACERS
#ifndef ALLOW_LONGSTEP
         IF ( usePTRACERS ) THEN
           IF ( .NOT.implicitDiffusion ) THEN
             CALL PTRACERS_CALC_DIFF(
     I            bi,bj,iMin,iMax,jMin,jMax,k,
     I            maskUp,
     O            kappaRTr,
     I            myThid)
           ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE kappaRTr(:,:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
# endif /* ALLOW_AUTODIFF_TAMC */
           CALL PTRACERS_INTEGRATE(
     I          bi,bj,k,
     I          xA, yA, maskUp, uFld, vFld, wFld,
     I          uTrans, vTrans, rTrans, rTransKp1,
     I          kappaRTr,
     U          fVerP,
     I          myTime,myIter,myThid)
         ENDIF
#endif /*ALLOW_LONGSTEP */
#endif /* ALLOW_PTRACERS */

C--      Freeze water
C  this bit of code is left here for backward compatibility.
C  freezing at surface level has been moved to DO_OCEANIC_PHYS
         IF ( useOldFreezing .AND. .NOT. useSEAICE
     &       .AND. .NOT.(useThSIce.AND.k.EQ.1) ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE gT(:,:,k,bi,bj) = comlev1_bibj_k
CADJ &   , key = kkey, byte = isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
            CALL FREEZE( bi, bj, iMin, iMax, jMin, jMax, k, myThid )
         ENDIF

C--     end of thermodynamic k loop (Nr:1)
        ENDDO

#ifdef ALLOW_DOWN_SLOPE
        IF ( tempStepping .AND. useDOWN_SLOPE ) THEN
          IF ( usingPCoords ) THEN
            CALL DWNSLP_APPLY(
     I                  GAD_TEMPERATURE, bi, bj, kSurfC,
     I                  recip_drF, recip_hFacC, recip_rA,
     I                  dTtracerLev,
     I                  theta,
     U                  gT,
     I                  myTime, myIter, myThid )
          ELSE
            CALL DWNSLP_APPLY(
     I                  GAD_TEMPERATURE, bi, bj, kLowC,
     I                  recip_drF, recip_hFacC, recip_rA,
     I                  dTtracerLev,
     I                  theta,
     U                  gT,
     I                  myTime, myIter, myThid )
          ENDIF
        ENDIF
        IF ( saltStepping .AND. useDOWN_SLOPE ) THEN
          IF ( usingPCoords ) THEN
            CALL DWNSLP_APPLY(
     I                  GAD_SALINITY, bi, bj, kSurfC,
     I                  recip_drF, recip_hFacC, recip_rA,
     I                  dTtracerLev,
     I                  salt,
     U                  gS,
     I                  myTime, myIter, myThid )
          ELSE
            CALL DWNSLP_APPLY(
     I                  GAD_SALINITY, bi, bj, kLowC,
     I                  recip_drF, recip_hFacC, recip_rA,
     I                  dTtracerLev,
     I                  salt,
     U                  gS,
     I                  myTime, myIter, myThid )
          ENDIF
        ENDIF
#ifdef ALLOW_PTRACERS
#ifndef ALLOW_LONGSTEP
        IF ( usePTRACERS .AND. useDOWN_SLOPE ) THEN
          CALL PTRACERS_DWNSLP_APPLY(
     I                  bi, bj, myTime, myIter, myThid )
        ENDIF
#endif /*ALLOW_LONGSTEP */
#endif /* ALLOW_PTRACERS */
#endif /* ALLOW_DOWN_SLOPE */

C       All explicit advection/diffusion/sources should now be
C       done. The updated tracer field is in gPtr. Accumalate
C       explicit tendency and also reset gPtr to initial tracer
C       field for implicit matrix calculation

#ifdef ALLOW_MATRIX
        IF (useMATRIX)
     &    CALL MATRIX_STORE_TENDENCY_EXP(bi,bj, myTime,myIter,myThid)
#endif

        iMin = 1
        iMax = sNx
        jMin = 1
        jMax = sNy

C--     Implicit vertical advection & diffusion
        IF ( tempStepping .AND. implicitDiffusion ) THEN
          CALL CALC_3D_DIFFUSIVITY(
     I         bi,bj,iMin,iMax,jMin,jMax,
     I         GAD_TEMPERATURE, useGMredi, useKPP,
     O         kappaRk,
     I         myThid)
        ENDIF
#ifdef INCLUDE_IMPLVERTADV_CODE
        IF ( tempImplVertAdv ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE kappaRk(:,:,:) = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE gT(:,:,:,bi,bj) = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE wvel(:,:,:,bi,bj) = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE theta(:,:,:,bi,bj) = comlev1_bibj , key=itdkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
          CALL GAD_IMPLICIT_R(
     I         tempImplVertAdv, tempVertAdvScheme, GAD_TEMPERATURE,
     I         dTtracerLev,
     I         kappaRk, wVel, theta,
     U         gT,
     I         bi, bj, myTime, myIter, myThid )
        ELSEIF ( tempStepping .AND. implicitDiffusion ) THEN
#else /* INCLUDE_IMPLVERTADV_CODE */
        IF     ( tempStepping .AND. implicitDiffusion ) THEN
#endif /* INCLUDE_IMPLVERTADV_CODE */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE kappaRk(:,:,:) = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE gT(:,:,:,bi,bj) = comlev1_bibj , key=itdkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         GAD_TEMPERATURE, kappaRk, recip_hFacC,
     U         gT,
     I         myThid )
        ENDIF

#ifdef ALLOW_TIMEAVE
        useVariableK = useKPP .OR. usePP81 .OR. useMY82 .OR. useGGL90
     &       .OR. useGMredi .OR. ivdc_kappa.NE.0.
        IF (taveFreq.GT.0. .AND. useVariableK ) THEN
         IF (implicitDiffusion) THEN
           CALL TIMEAVE_CUMUL_DIF_1T(TdiffRtave, gT, kappaRk,
     I                        Nr, 3, deltaTclock, bi, bj, myThid)
c        ELSE
c          CALL TIMEAVE_CUMUL_DIF_1T(TdiffRtave, theta, kappaRT,
c    I                        Nr, 3, deltaTclock, bi, bj, myThid)
         ENDIF
        ENDIF
#endif /* ALLOW_TIMEAVE */

        IF ( saltStepping .AND. implicitDiffusion ) THEN
          CALL CALC_3D_DIFFUSIVITY(
     I         bi,bj,iMin,iMax,jMin,jMax,
     I         GAD_SALINITY, useGMredi, useKPP,
     O         kappaRk,
     I         myThid)
        ENDIF

#ifdef INCLUDE_IMPLVERTADV_CODE
        IF ( saltImplVertAdv ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE kappaRk(:,:,:) = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE gS(:,:,:,bi,bj) = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE wvel(:,:,:,bi,bj) = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE salt(:,:,:,bi,bj) = comlev1_bibj , key=itdkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
          CALL GAD_IMPLICIT_R(
     I         saltImplVertAdv, saltVertAdvScheme, GAD_SALINITY,
     I         dTtracerLev,
     I         kappaRk, wVel, salt,
     U         gS,
     I         bi, bj, myTime, myIter, myThid )
        ELSEIF ( saltStepping .AND. implicitDiffusion ) THEN
#else /* INCLUDE_IMPLVERTADV_CODE */
        IF     ( saltStepping .AND. implicitDiffusion ) THEN
#endif /* INCLUDE_IMPLVERTADV_CODE */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE kappaRk(:,:,:) = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE gS(:,:,:,bi,bj) = comlev1_bibj , key=itdkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         GAD_SALINITY, kappaRk, recip_hFacC,
     U         gS,
     I         myThid )
        ENDIF

#ifdef ALLOW_PTRACERS
#ifndef ALLOW_LONGSTEP
        IF     ( usePTRACERS ) THEN
C--     Vertical advection/diffusion (implicit) for passive tracers
C       Also apply open boundary conditions for each passive tracer
           CALL PTRACERS_IMPLICIT(
     U                             kappaRk,
     I                             bi, bj, myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_LONGSTEP */
#endif /* ALLOW_PTRACERS */

#ifdef   ALLOW_OBCS
C--      Apply open boundary conditions
        IF ( useOBCS ) THEN
           CALL OBCS_APPLY_TS( bi, bj, 0, gT, gS, myThid )
        ENDIF
#endif   /* ALLOW_OBCS */

#endif /* SINGLE_LAYER_MODE */

C--   end bi,bj loops.
       ENDDO
      ENDDO

#ifdef ALLOW_DEBUG
      IF ( debugLevel.GE.debLevD ) THEN
       CALL DEBUG_STATS_RL(Nr,uVel,'Uvel (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,vVel,'Vvel (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,wVel,'Wvel (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,theta,'Theta (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,salt,'Salt (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,gT,'Gt (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,gS,'Gs (THERMODYNAMICS)',myThid)
#ifndef ALLOW_ADAMSBASHFORTH_3
       CALL DEBUG_STATS_RL(Nr,gtNm1,'GtNm1 (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,gsNm1,'GsNm1 (THERMODYNAMICS)',myThid)
#endif
#ifdef ALLOW_PTRACERS
#ifndef ALLOW_LONGSTEP
       IF ( usePTRACERS ) THEN
         CALL PTRACERS_DEBUG(myThid)
       ENDIF
#endif /* ALLOW_LONGSTEP */
#endif /* ALLOW_PTRACERS */
      ENDIF
#endif /* ALLOW_DEBUG */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('THERMODYNAMICS',myThid)
#endif

#endif /* ALLOW_GENERIC_ADVDIFF */

      RETURN
      END
