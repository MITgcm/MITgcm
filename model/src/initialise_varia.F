C $Header: /u/gcmpack/MITgcm/model/src/initialise_varia.F,v 1.68 2011/03/15 00:23:38 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INITIALISE_VARIA
C     !INTERFACE:
      SUBROUTINE INITIALISE_VARIA( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INITIALISE_VARIA
C     | o Set the initial conditions for dynamics variables
C     |   and time dependent arrays
C     *==========================================================*
C     | This routine reads/writes  data from an input file and
C     | from various binary files.
C     | Each thread invokes an instance of this routine as does
C     | each process in a multi-process parallel environment like
C     | MPI.
C     *==========================================================*
C     \ev

C     !CALLING SEQUENCE:
C     INITIALISE_VARIA
C       |
C #ifdef ALLOW_AUTODIFF_TAMC
C       |-- INI_HFAC           \
C       |-- INI_DEPTHS          \
C       |-- INI_MASKS_ETC        } ALLOW_DEPTH_CONTROL case
C       |-- INI_LINEAR_PHISURF  /
C       |
C       |-- INI_NLFS_VARS
C       |
C       |-- INI_CG2D
C       |
C       |-- CTRL_DEPTH_INI      \
C       |-- UPDATE_MASKS_ETC     } ALLOW_DEPTH_CONTROL case
C       |-- UPDATE_CG2D         /
C       |
C       |-- INI_CG3D
C #else
C       |-- INI_NLFS_VARS
C #endif
C       |-- INI_DYNVARS
C       |-- INI_NH_VARS
C       |
C       |-- INI_FIELDS
C       |
C       |-- INI_MIXING
C       |
C       |-- TAUEDDY_INIT_VARIA
C       |
C       |-- INI_FORCING
C       |
C       |-- INI_AUTODIFF
C       |
C       |-- PACKAGES_INIT_VARIABLES
C       |
C       |-- CONVECTIVE_ADJUSTMENT_INI
C       |
C       |-- CALC_R_STAR
C       |-- UPDATE_R_STAR
C       |-- UPDATE_SIGMA
C       |-- CALC_SURF_DR
C       |-- UPDATE_SURF_DR
C       |
C       |-- UPDATE_CG2D
C       |
C       |-- INTEGR_CONTINUITY
C       |
C       |-- UPDATE_ETAH
C       |
C       |-- CALC_R_STAR
C       |-- CALC_SURF_DR
C       |
C       |-- STATE_SUMMARY
C       |
C       |-- MONITOR
C       |
C       |-- DO_STATEVARS_TAVE
C       |
C       |-- DO_THE_MODEL_IO

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('INITIALISE_VARIA',myThid)
#endif

#ifdef ALLOW_AUTODIFF_TAMC

      nIter0 = NINT( (startTime-baseTime)/deltaTClock )

# ifdef ALLOW_DEPTH_CONTROL
C--   Intialize the hFacC for TAF/TAMC
      CALL INI_HFAC( myThid )
      CALL INI_DEPTHS( myThid )
      CALL INI_MASKS_ETC( myThid )

C--   Set Bo_surf => define the Linear Relation: Phi_surf(eta)
# ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_LINEAR_PHISURF',myThid)
# endif
      CALL INI_LINEAR_PHISURF( myThid )
# endif /* ALLOW_DEPTH_CONTROL */

# ifdef NONLIN_FRSURF
C--   Initialise Non-Lin FreeSurf variables:
      CALL INI_NLFS_VARS( myThid )
# endif /* NONLIN_FRSURF */

C--   Set laplace operators for use in 2D conjugate gradient solver.
# ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_CG2D',myThid)
# endif
      CALL INI_CG2D( myThid )

# ifdef ALLOW_DEPTH_CONTROL
C--   get control parameter depth
      CALL CTRL_DEPTH_INI( myThid )
C--   Re-calculate hFacS/W and some other parameters from hFacC
      CALL UPDATE_MASKS_ETC( myThid )
C--   Update laplace operators for use in 2D conjugate gradient solver.
      CALL UPDATE_CG2D( startTime, nIter0, myThid )
# endif /* ALLOW_DEPTH_CONTROL */

# ifdef ALLOW_NONHYDROSTATIC
C--   Set laplace operators for use in 3D conjugate gradient solver.
#  ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_CG3D',myThid)
#  endif
      CALL INI_CG3D( myThid )
# endif

#else /* ALLOW_AUTODIFF_TAMC */

# ifdef NONLIN_FRSURF
C--   Initialise Non-Lin FreeSurf variables:
      CALL INI_NLFS_VARS( myThid )
# endif /* NONLIN_FRSURF */

#endif /* ALLOW_AUTODIFF_TAMC */

C--   Initialize DYNVARS arrays (state fields + G terms: Gu,Gv,...) to zero [always]
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_DYNVARS',myThid)
#endif
      CALL INI_DYNVARS( myThid )

C--   Initialize NH_VARS arrays to zero [always]
#ifdef ALLOW_NONHYDROSTATIC
      CALL INI_NH_VARS( myThid )
#endif

C--   Initialise model fields.
C     Starting values of U, V, W, temp., salt. and tendency terms
C     are set here. Fields are either set to default or read from
C     stored files.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_FIELDS',myThid)
#endif
      CALL INI_FIELDS( myThid )

C--   Initialise 3-dim. diffusivities
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_MIXING',myThid)
#endif
      CALL INI_MIXING( myThid )

#ifdef ALLOW_EDDYPSI
C--  Initialise eddy diffusivities
      CALL TAUEDDY_INIT_VARIA( myThid )
#endif

C--   Initialise model forcing fields.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_FORCING',myThid)
#endif
      CALL INI_FORCING( myThid )

#ifdef ALLOW_AUTODIFF_TAMC
C--   Initialise active fields to help TAMC
      CALL INI_AUTODIFF( myThid )
#endif

C--   Initialize variable data for packages
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('PACKAGES_INIT_VARIABLES',myThid)
#endif
      CALL PACKAGES_INIT_VARIABLES( myThid )

c#ifndef ALLOW_AUTODIFF_TAMC
c     IF ( usePickupBeforeC35 .AND. startTime .NE. baseTime ) THEN
C-- IMPORTANT : Need to activate the following call to restart from a pickup
C     file written by MITgcmUV_checkpoint34 (Feb-08, 2001) or earlier.
C-  Disable this option on Jan-09, 2007.
c      CALL THE_CORRECTION_STEP(startTime, nIter0, myThid)
c     ENDIF
c#endif

#ifndef ALLOW_AUTODIFF_WHTAPEIO
C--   Initial conditions are convectively adjusted (for historical reasons)
      IF ( startTime .EQ. baseTime .AND. cAdjFreq .NE. 0. ) THEN
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('CONVECTIVE_ADJUSTMENT_INI',myThid)
#endif
CADJ loop = parallel
        DO bj = myByLo(myThid), myByHi(myThid)
CADJ loop = parallel
         DO bi = myBxLo(myThid), myBxHi(myThid)
           CALL CONVECTIVE_ADJUSTMENT_INI(
     I                     bi, bj, startTime, nIter0, myThid )
         ENDDO
        ENDDO
      ENDIF
#endif

#ifdef NONLIN_FRSURF
C--   Compute the surface level thickness <-- function of etaH(n)
C     and modify hFac(C,W,S) accordingly :
# ifndef DISABLE_RSTAR_CODE
      IF ( select_rStar.NE.0 )
     &  CALL CALC_R_STAR(etaH, startTime, -1 , myThid )
# endif /* DISABLE_RSTAR_CODE */
      IF (nonlinFreeSurf.GT.0) THEN
       IF ( select_rStar.GT.0 ) THEN
# ifndef DISABLE_RSTAR_CODE
        CALL UPDATE_R_STAR( .TRUE., startTime, nIter0, myThid )
# endif /* DISABLE_RSTAR_CODE */
       ELSEIF ( selectSigmaCoord.NE.0 ) THEN
# ifndef DISABLE_SIGMA_CODE
        CALL UPDATE_SIGMA( etaH, startTime, nIter0, myThid )
# endif /* DISABLE_RSTAR_CODE */
       ELSE
        CALL CALC_SURF_DR(etaH, startTime, -1 , myThid )
        CALL UPDATE_SURF_DR( .TRUE., startTime, nIter0, myThid )
       ENDIF
      ENDIF
C-    update also CG2D matrix (and preconditioner)
      IF ( nonlinFreeSurf.GT.2) THEN
        CALL UPDATE_CG2D( startTime, nIter0, myThid )
      ENDIF
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INTEGR_CONTINUITY',myThid)
#endif
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C--   Integrate continuity vertically for vertical velocity
        CALL INTEGR_CONTINUITY( bi, bj, uVel, vVel,
     I                          startTime, nIter0, myThid )

       ENDDO
      ENDDO

#ifdef EXACT_CONSERV
      IF ( exactConserv ) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('UPDATE_ETAH',myThid)
#endif
C--   Update etaH(n) :
         CALL UPDATE_ETAH( startTime, nIter0, myThid )
      ENDIF
#endif /* EXACT_CONSERV */

#ifdef NONLIN_FRSURF
      IF ( select_rStar.NE.0 ) THEN
#ifndef DISABLE_RSTAR_CODE
C--   r* : compute the future level thickness according to etaH(n+1)
          CALL CALC_R_STAR(etaH, startTime, nIter0, myThid )
#endif
      ELSEIF ( nonlinFreeSurf.GT.0) THEN
C--   compute the future surface level thickness according to etaH(n+1)
          CALL CALC_SURF_DR(etaH, startTime, nIter0, myThid )
      ENDIF
#endif /* NONLIN_FRSURF */

c     IF ( nIter0.EQ.0 .AND. staggerTimeStep ) THEN
C--    Filter initial T & S fields if staggerTimeStep
C       (only for backward compatibility ; to be removed later)
#ifdef ALLOW_SHAP_FILT
c      IF ( useSHAP_FILT .AND. shap_filt_TrStagg ) THEN
c       CALL SHAP_FILT_APPLY_TS(theta,salt,startTime,nIter0,myThid)
c      ENDIF
#endif
#ifdef ALLOW_ZONAL_FILT
c      IF ( useZONAL_FILT .AND. zonal_filt_TrStagg ) THEN
c       CALL ZONAL_FILT_APPLY_TS( theta, salt, myThid )
c      ENDIF
#endif
c     ENDIF

#ifdef ALLOW_GRIDALT
      IF (useGRIDALT) THEN
         CALL TIMER_START('GRIDALT_UPDATE  [INITIALISE_VARIA]',myThid)
         CALL GRIDALT_UPDATE(myThid)
         CALL TIMER_STOP ('GRIDALT_UPDATE  [INITIALISE_VARIA]',myThid)
      ENDIF
#endif

C--   Fill in overlap regions for wVel :
      _EXCH_XYZ_RL(wVel,myThid)

C--   Finally summarise the model state
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('STATE_SUMMARY',myThid)
#endif
      CALL STATE_SUMMARY( myThid )

#ifdef ALLOW_MONITOR
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('MONITOR',myThid)
#endif
C--   Check status of initial state (statistics, cfl, etc...)
      CALL MONITOR( startTime, nIter0, myThid )
#endif /* ALLOW_MONITOR */

#ifdef ALLOW_TIMEAVE
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('DO_STATEVARS_TAVE',myThid)
#endif
C--   Initialise time-average arrays with initial state values
      CALL DO_STATEVARS_TAVE( startTime, nIter0, myThid )
#endif

C--   Dump initial state to files
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('DO_THE_MODEL_IO',myThid)
#endif
      CALL DO_THE_MODEL_IO( .FALSE., startTime, nIter0, myThid )

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('INITIALISE_VARIA',myThid)
#endif

C--   Check barrier synchronization:
      CALL BAR_CHECK( 4, myThid )

      RETURN
      END
