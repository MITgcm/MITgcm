C $Header: /u/gcmpack/MITgcm/model/src/thermodynamics.F,v 1.148 2014/04/04 20:54:11 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#if (defined ALLOW_PTRACERS) && (!defined ALLOW_LONGSTEP)
# define DO_PTRACERS_HERE
#endif

#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_GMREDI
#  include "GMREDI_OPTIONS.h"
# endif
# ifdef ALLOW_KPP
#  include "KPP_OPTIONS.h"
# endif
#endif /* ALLOW_AUTODIFF */

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
#include "SURFACE.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif
#ifdef DO_PTRACERS_HERE
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
# include "PTRACERS_FIELDS.h"
#endif
c#ifdef ALLOW_TIMEAVE
c# include "TIMEAVE_STATV.h"
c#endif

#ifdef ALLOW_AUTODIFF
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
# ifdef ALLOW_EBM
#  include "EBM.h"
# endif
# ifdef ALLOW_SALT_PLUME
#  include "SALT_PLUME.h"
# endif
#endif /* ALLOW_AUTODIFF */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GENERIC_ADVDIFF
C     !LOCAL VARIABLES:
C     == Local variables
C     uFld,vFld,wFld :: Local copy of velocity field (3 components)
C     kappaRk        :: Total diffusion in vertical, all levels, 1 tracer
C     recip_hFacNew  :: reciprocal of futur time-step hFacC
C     useVariableK   :: T when vertical diffusion is not constant
C     iMin, iMax     :: Ranges and sub-block indices on which calculations
C     jMin, jMax        are applied.
C     bi, bj         :: Tile indices
C     i, j, k        :: loop indices
      _RL uFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL vFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL wFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL kappaRk (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RS recip_hFacNew(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
c     INTEGER iMin, iMax
c     INTEGER jMin, jMax
      INTEGER bi, bj
      INTEGER i, j, k
#ifdef ALLOW_ADAMSBASHFORTH_3
      INTEGER iterNb, m1, m2
      _RL tmpFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
#ifdef ALLOW_TIMEAVE
c     LOGICAL useVariableK
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
#ifdef ALLOW_AUTODIFF
      TsurfCor = 0. _d 0
      SsurfCor = 0. _d 0
#endif
      IF ( linFSConserveTr ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta,salt,wvel = comlev1, key = ikey_dynamics, byte=isbyte
#endif
       CALL CALC_WSURF_TR( theta, salt, wVel,
     &                     myTime, myIter, myThid )
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
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

        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           recip_hFacNew(i,j,k) = 0. _d 0
C This is currently also used by IVDC and Diagnostics
           kappaRk(i,j,k)    = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

C--     Compute new reciprocal hFac for implicit calculation
#ifdef NONLIN_FRSURF
        IF ( nonlinFreeSurf.GT.0 ) THEN
         IF ( select_rStar.GT.0 ) THEN
# ifndef DISABLE_RSTAR_CODE
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             recip_hFacNew(i,j,k) = recip_hFacC(i,j,k,bi,bj)
     &                            / rStarExpC(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
# endif /* DISABLE_RSTAR_CODE */
         ELSEIF ( selectSigmaCoord.NE.0 ) THEN
# ifndef DISABLE_SIGMA_CODE
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             recip_hFacNew(i,j,k) = recip_hFacC(i,j,k,bi,bj)
     &        /( 1. _d 0 + dEtaHdt(i,j,bi,bj)*deltaTFreeSurf
     &                    *dBHybSigF(k)*recip_drF(k)
     &                    *recip_hFacC(i,j,k,bi,bj)
     &         )
            ENDDO
           ENDDO
          ENDDO
# endif /* DISABLE_RSTAR_CODE */
         ELSE
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             IF ( k.EQ.kSurfC(i,j,bi,bj) ) THEN
              recip_hFacNew(i,j,k) = 1. _d 0 / hFac_surfC(i,j,bi,bj)
             ELSE
              recip_hFacNew(i,j,k) = recip_hFacC(i,j,k,bi,bj)
             ENDIF
            ENDDO
           ENDDO
          ENDDO
         ENDIF
        ELSE
#endif /* NONLIN_FRSURF */
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             recip_hFacNew(i,j,k) = _recip_hFacC(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
#ifdef NONLIN_FRSURF
        ENDIF
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_ADAMSBASHFORTH_3
C-      Apply AB on T,S :
        iterNb = myIter
        IF (staggerTimeStep) iterNb = myIter - 1
        m1 = 1 + MOD(iterNb+1,2)
        m2 = 1 + MOD( iterNb ,2)
C       compute T^n+1/2 (stored in gtNm) extrapolating T forward in time
        IF ( AdamsBashforth_T ) CALL ADAMS_BASHFORTH3(
     I                                  bi, bj, 0, Nr,
     U                                  theta, gtNm, tmpFld,
     I                                  tempStartAB, iterNb, myThid )
C       compute S^n+1/2 (stored in gsNm) extrapolating S forward in time
        IF ( AdamsBashforth_S ) CALL ADAMS_BASHFORTH3(
     I                                  bi, bj, 0, Nr,
     U                                  salt, gsNm, tmpFld,
     I                                  saltStartAB, iterNb, myThid )
#endif /* ALLOW_ADAMSBASHFORTH_3 */

C--   Set up 3-D velocity field that we use to advect tracers:
C-    just do a local copy:
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uFld(i,j,k) = uVel(i,j,k,bi,bj)
           vFld(i,j,k) = vVel(i,j,k,bi,bj)
           wFld(i,j,k) = wVel(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_GMREDI
C-    add Bolus velocity to Eulerian-mean velocity:
        IF (useGMRedi) THEN
          CALL  GMREDI_RESIDUAL_FLOW(
     U                  uFld, vFld, wFld,
     I                  bi, bj, myIter, myThid )
        ENDIF
#endif /* ALLOW_GMREDI */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE recip_hFacNew(:,:,:) = comlev1_bibj , key=itdkey, byte=isbyte
CADJ STORE uFld (:,:,:)       = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE vFld (:,:,:)       = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE wFld (:,:,:)       = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE theta(:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE salt (:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
#  ifdef ALLOW_ADAMSBASHFORTH_3
CADJ STORE gtNm(:,:,:,bi,bj,1) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE gtNm(:,:,:,bi,bj,2) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE gsNm(:,:,:,bi,bj,1) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE gsNm(:,:,:,bi,bj,2) = comlev1_bibj, key=itdkey, byte=isbyte
#  else
CADJ STORE gtNm1(:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE gsNm1(:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
#  endif
# ifdef ALLOW_SALT_PLUME
CADJ STORE saltPlumeFlux(:,:,bi,bj)   =
CADJ &     comlev1_bibj, key=itdkey,kind = isbyte
CADJ STORE saltPlumeDepth(:,:,bi,bj)   =
CADJ &     comlev1_bibj, key=itdkey,kind = isbyte
# endif
# if ((defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)) && (defined ALLOW_GMREDI)
#  ifdef GM_NON_UNITY_DIAGONAL
CADJ STORE kux(:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE kvy(:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
#  endif
#  ifdef GM_EXTRA_DIAGONAL
CADJ STORE kuz(:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
CADJ STORE kvz(:,:,:,bi,bj) = comlev1_bibj, key=itdkey, byte=isbyte
#  endif
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Calculate active tracer tendencies (gT,gS,...)
C       and step forward, storing result in gT, gS, etc.

        IF ( tempStepping ) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('TEMP_INTEGRATE',myThid)
#endif
          CALL TEMP_INTEGRATE(
     I         bi, bj, recip_hFacNew,
     I         uFld, vFld, wFld,
     U         kappaRk,
     I         myTime, myIter, myThid )
        ENDIF

        IF ( saltStepping ) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('SALT_INTEGRATE',myThid)
#endif
          CALL SALT_INTEGRATE(
     I         bi, bj, recip_hFacNew,
     I         uFld, vFld, wFld,
     U         kappaRk,
     I         myTime, myIter, myThid )
        ENDIF

#ifdef DO_PTRACERS_HERE
C--     Calculate passive tracer tendencies
C       and step forward, storing result in gPtr
C       Also apply open boundary conditions for each passive tracer
        IF ( usePTRACERS ) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('PTRACERS_INTEGRATE',myThid)
#endif
           CALL PTRACERS_INTEGRATE(
     I          bi, bj, recip_hFacNew,
     I          uFld, vFld, wFld,
     U          kappaRk,
     I          myTime, myIter, myThid )
        ENDIF
#endif /* DO_PTRACERS_HERE */

#ifdef   ALLOW_OBCS
C--   Apply open boundary conditions
        IF ( useOBCS ) THEN
           CALL OBCS_APPLY_TS( bi, bj, 0, gT, gS, myThid )
        ENDIF
#endif   /* ALLOW_OBCS */

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
#ifdef DO_PTRACERS_HERE
       IF ( usePTRACERS ) THEN
         CALL PTRACERS_DEBUG(myThid)
       ENDIF
#endif /* DO_PTRACERS_HERE */
      ENDIF
#endif /* ALLOW_DEBUG */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('THERMODYNAMICS',myThid)
#endif

#endif /* ALLOW_GENERIC_ADVDIFF */

      RETURN
      END
