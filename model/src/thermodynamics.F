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
# ifdef ALLOW_SALT_PLUME
#  include "SALT_PLUME_OPTIONS.h"
# endif
#endif /* ALLOW_AUTODIFF */
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

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
#include "FFIELDS.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif
#ifdef DO_PTRACERS_HERE
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
# include "PTRACERS_FIELDS.h"
#endif

#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_AUTODIFF_TAMC
#  include "tamc.h"
# endif
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

#ifdef ALLOW_TAPENADE
# ifdef ALLOW_GENERIC_ADVDIFF
#  include "GAD_SOM_VARS.h"
# endif
#endif /* ALLOW_TAPENADE */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GENERIC_ADVDIFF
# ifdef ALLOW_MONITOR
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
# endif /* ALLOW_MONITOR */

C     !LOCAL VARIABLES:
C     == Local variables
C     uFld,vFld,wFld :: Local copy of velocity field (3 components)
C     kappaRk        :: Total diffusion in vertical, all levels, 1 tracer
C     recip_hFacNew  :: reciprocal of futur time-step hFacC
C     bi, bj         :: Tile indices
C     i, j, k        :: loop indices
      _RL uFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL vFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL wFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL kappaRk (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RS recip_hFacNew(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER bi, bj
      INTEGER i, j, k
# ifdef ALLOW_MONITOR
      LOGICAL monOutputCFL
      _RL wrTime
      _RL trAdvCFL(3,nSx,nSy)
# endif /* ALLOW_MONITOR */
#ifdef ALLOW_AUTODIFF_TAMC
C     tkey :: tape key (tile dependent)
      INTEGER tkey
#endif
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('THERMODYNAMICS',myThid)
#endif

# ifdef ALLOW_MONITOR
      monOutputCFL = .FALSE.
      IF ( monitorSelect.GE.2 ) THEN
        wrTime = myTime
        IF ( .NOT.staggerTimeStep ) wrTime = myTime + deltaTClock
        monOutputCFL =
     &       DIFFERENT_MULTIPLE( monitorFreq, wrTime, deltaTClock )
      ENDIF
# endif /* ALLOW_MONITOR */

#ifdef ALLOW_AUTODIFF_TAMC
C--   dummy statement to end declaration part
      tkey = 1

C--   HPF directive to help TAF
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
#ifdef ALLOW_LAYERS
      IF ( useLayers ) THEN
        CALL LAYERS_WSURF_TR( theta, salt, wVel,
     &                     myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_LAYERS */

#ifdef DO_PTRACERS_HERE
#ifdef ALLOW_AUTODIFF
      DO k=1,PTRACERS_numInUse
        meanSurfCorPTr(k) = 0.0 _d 0
      ENDDO
#endif
      IF ( PTRACERS_calcSurfCor ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ptracer = comlev1, key = ikey_dynamics, byte=isbyte
#endif
       CALL PTRACERS_CALC_WSURF_TR(wVel,myTime,myIter,myThid)
      ENDIF
#endif /* DO_PTRACERS_HERE */

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
        tkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
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
          CALL GMREDI_RESIDUAL_FLOW(
     U                  uFld, vFld, wFld,
     I                  bi, bj, myIter, myThid )
        ENDIF
#endif /* ALLOW_GMREDI */
#ifdef ALLOW_MONITOR
        IF ( monOutputCFL  ) THEN
          CALL MON_CALC_ADVCFL_TILE( Nr, bi, bj,
     I                         uFld, vFld, wFld, dTtracerLev,
     O                         trAdvCFL(1,bi,bj),
     I                         myIter, myThid )
c        WRITE(standardMessageUnit,'(A,I8,2I3,A,1P3E14.6)')
c    &     ' trAdv_CFL: it,bi,bj=', myIter,bi,bj,
c    &     ' , CFL =', (trAdvCFL(i,bi,bj),i=1,3)
        ENDIF
#endif /* ALLOW_MONITOR */

C-    Apply AB on T,S : moved inside TEMP/SALT_INTEGRATE

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE recip_hFacNew(:,:,:) = comlev1_bibj, key=tkey, byte=isbyte
CADJ STORE uFld (:,:,:)         = comlev1_bibj, key=tkey, byte=isbyte
CADJ STORE vFld (:,:,:)         = comlev1_bibj, key=tkey, byte=isbyte
CADJ STORE wFld (:,:,:)         = comlev1_bibj, key=tkey, byte=isbyte
CADJ STORE theta(:,:,:,bi,bj)   = comlev1_bibj, key=tkey, byte=isbyte
CADJ STORE salt (:,:,:,bi,bj)   = comlev1_bibj, key=tkey, byte=isbyte
# ifdef ALLOW_SALT_PLUME
CADJ STORE saltPlumeFlux(:,:,bi,bj)  = comlev1_bibj,key=tkey,kind=isbyte
CADJ STORE saltPlumeDepth(:,:,bi,bj) = comlev1_bibj,key=tkey,kind=isbyte
# endif
# if (defined NONLIN_FRSURF || defined ALLOW_DEPTH_CONTROL) \
        && defined ALLOW_GMREDI
CADJ STORE kux(:,:,:,bi,bj) = comlev1_bibj, key=tkey, byte=isbyte
CADJ STORE kvy(:,:,:,bi,bj) = comlev1_bibj, key=tkey, byte=isbyte
#  ifdef GM_EXTRA_DIAGONAL
CADJ STORE kuz(:,:,:,bi,bj) = comlev1_bibj, key=tkey, byte=isbyte
CADJ STORE kvz(:,:,:,bi,bj) = comlev1_bibj, key=tkey, byte=isbyte
#  endif
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Calculate active tracer tendencies and step forward in time.
C       Active tracer arrays are updated while adjustments (filters,
C       conv.adjustment) are applied later in TRACERS_CORRECTION_STEP.

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
C--     Calculate passive tracer tendencies and step forward in time.
C       Passive tracer arrays are updated while adjustments (filters,
C       conv.adjustment) are applied later in TRACERS_CORRECTION_STEP.
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
          CALL OBCS_APPLY_TS( bi, bj, 0, theta, salt, myThid )
        ENDIF
#endif   /* ALLOW_OBCS */

#ifdef ALLOW_FRICTION_HEATING
#ifdef ALLOW_DIAGNOSTICS
        IF ( addFrictionHeating .AND. useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL_RS( frictionHeating, 'HeatDiss',
     &                              0, Nr, 1, bi, bj, myThid )
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */
C-    Reset frictionHeating to zero
        IF ( addFrictionHeating .AND. .NOT.staggerTimeStep ) THEN
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              frictionHeating(i,j,k,bi,bj) = 0. _d 0
            ENDDO
           ENDDO
          ENDDO
        ENDIF
#endif /* ALLOW_FRICTION_HEATING */

C--   end bi,bj loops.
       ENDDO
      ENDDO

#ifdef ALLOW_MONITOR
      IF ( monOutputCFL ) THEN
        CALL MON_CALC_ADVCFL_GLOB(
     I                       trAdvCFL, myIter, myThid )
      ENDIF
#endif /* ALLOW_MONITOR */

#ifdef ALLOW_DEBUG
      IF ( debugLevel.GE.debLevD ) THEN
       CALL DEBUG_STATS_RL(Nr,uVel,'Uvel (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,vVel,'Vvel (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,wVel,'Wvel (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,theta,'Theta (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,salt,'Salt (THERMODYNAMICS)',myThid)
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
