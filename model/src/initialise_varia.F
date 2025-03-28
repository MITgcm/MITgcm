#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_TAPENADE
# ifdef ALLOW_SHELFICE
#  include "SHELFICE_OPTIONS.h"
# endif
# ifdef ALLOW_GMREDI
#  include "GMREDI_OPTIONS.h"
# endif
# ifdef ALLOW_EXF
#  include "EXF_OPTIONS.h"
# endif
# ifdef ALLOW_SEAICE
#  include "SEAICE_OPTIONS.h"
# endif
# ifdef ALLOW_ECCO
#  include "ECCO_OPTIONS.h"
# endif
c# ifdef ALLOW_THSICE
c# include "THSICE_OPTIONS.h"
c# endif
# ifdef ALLOW_BLING
#  include "BLING_OPTIONS.h"
# endif
#endif /* ALLOW_TAPENADE */

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
C #ifdef ALLOW_AUTODIFF
C       |-- INI_DEPTHS         \
C       |-- CTRL_DEPTH_INI      \
C       |-- UPDATE_MASKS_ETC     } ALLOW_DEPTH_CONTROL case
C       |-- UPDATE_CG2D         /
C #endif
C       |-- INI_NLFS_VARS
C       |-- INI_DYNVARS
C       |-- INI_NH_VARS
C       |-- INI_FFIELDS
C       |
C       |-- INI_FIELDS
C       |
C       |-- INI_MIXING
C       |
C       |-- TAUEDDY_INIT_VARIA
C       |
C       |-- INI_FORCING
C       |
C       |-- AUTODIFF_INIT_VARIA
C       |
C       |-- PACKAGES_INIT_VARIABLES
C       |
C       |-- COST_INIT_VARIA
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
#ifdef ALLOW_AUTODIFF
# include "GRID.h"
# include "FFIELDS.h"
# if ( defined ALLOW_CTRL && defined ALLOW_GENTIM2D_CONTROL && !defined ALLOW_TAPENADE )
#  include "CTRL_SIZE.h"
#  include "CTRL_DUMMY.h"
# endif
#endif /* ALLOW_AUTODIFF */

#ifdef ALLOW_TAPENADE
# ifdef ALLOW_GMREDI
#  include "GMREDI.h"
# endif
# ifdef ALLOW_CTRL
#  include "CTRL_SIZE.h"
#  include "CTRL.h"
#  include "CTRL_GENARR.h"
#  include "CTRL_DUMMY.h"
# endif
# ifdef ALLOW_CD_CODE
#  include "CD_CODE_VARS.h"
# endif
# ifdef ALLOW_DIC
#  include "DIC_VARS.h"
#  include "DIC_ATMOS.h"
#  include "DIC_LOAD.h"
# endif
# ifdef ALLOW_PTRACERS
#  include "PTRACERS_SIZE.h"
#  include "PTRACERS_FIELDS.h"
#  include "PTRACERS_PARAMS.h"
# endif
# ifdef ALLOW_GENERIC_ADVDIFF
#  include "GAD_SOM_VARS.h"
# endif
# ifdef ALLOW_EXF
#  include "EXF_PARAM.h"
#  include "EXF_INTERP_SIZE.h"
#  include "EXF_INTERP_PARAM.h"
#  include "EXF_FIELDS.h"
# endif
# ifdef ALLOW_STREAMICE
#  include "STREAMICE.h"
#  include "STREAMICE_CG.h"
# endif
# ifdef ALLOW_THSICE
#  include "THSICE_VARS.h"
# endif
# ifdef ALLOW_SEAICE
#  include "SEAICE_SIZE.h"
#  include "SEAICE.h"
# endif
# ifdef ALLOW_ECCO
#  include "ECCO_SIZE.h"
#  include "ECCO.h"
# endif
# ifdef ALLOW_BLING
#  include "BLING_VARS.h"
#  include "BLING_LOAD.h"
# endif
#endif /* ALLOW_TAPENADE */

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid

C     !LOCAL VARIABLES:
#ifdef INCLUDE_CONVECT_INI_CALL
      INTEGER bi,bj
#endif
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('INITIALISE_VARIA',myThid)
#endif

#ifdef ALLOW_AUTODIFF
      nIter0 = NINT( (startTime-baseTime)/deltaTClock )
#endif /* ALLOW_AUTODIFF */

#ifdef ALLOW_DEPTH_CONTROL
C--   Intialize the depth for TAF
      CALL INI_DEPTHS( myThid )
C--   Get control parameter depth
      CALL CTRL_DEPTH_INI( myThid )
C--   Re-calculate hFacS/W and some other parameters from hFacC
      CALL UPDATE_MASKS_ETC( myThid )
C--   Update laplace operators for use in 2D conjugate gradient solver.
      CALL UPDATE_CG2D( startTime, nIter0, myThid )
#endif /* ALLOW_DEPTH_CONTROL */

C--   Initialise Non-Lin FreeSurf variables:
      CALL INI_NLFS_VARS( myThid )

C--   Initialize DYNVARS arrays (state fields + G terms: Gu,Gv,...) to zero [always]
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_DYNVARS',myThid)
#endif
      CALL INI_DYNVARS( myThid )

C--   Initialize NH_VARS arrays to zero [always]
#ifdef ALLOW_NONHYDROSTATIC
      CALL INI_NH_VARS( myThid )
#endif

C--   Initialize FFIELDS arrays to zero [always]
      CALL INI_FFIELDS( myThid )

C--   Initialise model fields.
C     Starting values of U, V, W, temp., salt. and tendency terms
C     are set here. Fields are either set to default or read from
C     stored files.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_FIELDS',myThid)
#endif
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE phi0surf = tapelev_init, key = 1
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

#ifdef ALLOW_AUTODIFF
C--   Initialise active fields to help TAF
      if (useAUTODIFF) CALL AUTODIFF_INIT_VARIA( myThid )
#endif

C--   Initialize variable data for packages
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('PACKAGES_INIT_VARIABLES',myThid)
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# ifdef NONLIN_FRSURF
CADJ STORE recip_hFacC = tapelev_init, key = 1
# endif
# ifdef ALLOW_GENTIM2D_CONTROL
C     It not clear, why this is needed and hopefully we will find a
C     better way to avoid the recomputation warning.
CADJ STORE xx_gentim2d_dummy = dummytape, key = 1
# endif
#endif
      CALL PACKAGES_INIT_VARIABLES( myThid )

#ifdef ALLOW_COST
C--   Initialise the cost function (moved out of packages_init_variables to
C     here to prevent resetting cost-funct in adinitialise_varia recomput.)
      CALL COST_INIT_VARIA( myThid )
#endif /* ALLOW_COST */

c#ifndef ALLOW_AUTODIFF
c     IF ( usePickupBeforeC35 .AND. startTime .NE. baseTime ) THEN
C-- IMPORTANT : Need to activate the following call to restart from a pickup
C     file written by MITgcmUV_checkpoint34 (Feb-08, 2001) or earlier.
C-  Disable this option on Jan-09, 2007.
c      CALL THE_CORRECTION_STEP(startTime, nIter0, myThid)
c     ENDIF
c#endif

#ifdef INCLUDE_CONVECT_INI_CALL
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
#endif /* INCLUDE_CONVECT_INI_CALL */

#ifdef NONLIN_FRSURF
C--   Compute the surface level thickness <-- function of etaH(n)
C     and modify hFac(C,W,S) accordingly :
# ifndef DISABLE_RSTAR_CODE
      IF ( select_rStar.NE.0 )
     &  CALL CALC_R_STAR(etaH, startTime, -1 , myThid )
# endif /* DISABLE_RSTAR_CODE */
      IF ( nonlinFreeSurf.GT.0 ) THEN
       IF ( select_rStar.GT.0 ) THEN
# ifndef DISABLE_RSTAR_CODE
        CALL UPDATE_R_STAR( .TRUE., startTime, nIter0, myThid )
# endif /* DISABLE_RSTAR_CODE */
       ELSEIF ( selectSigmaCoord.NE.0 ) THEN
# ifndef DISABLE_SIGMA_CODE
        CALL UPDATE_SIGMA( etaH, startTime, nIter0, myThid )
# endif /* DISABLE_SIGMA_CODE */
       ELSE
        CALL CALC_SURF_DR(etaH, startTime, -1 , myThid )
#ifdef ALLOW_AUTODIFF_TAMC
C     These fields are only updated in the surface layer and TAF gets
C     confused about it so that their contribution is not always added
C     to the hfac*_ad lateron. This directive fixes that.
CADJ INCOMPLETE recip_hFacC, recip_hFacW, recip_hFacS
#endif /* ALLOW_AUTODIFF_TAMC */
        CALL UPDATE_SURF_DR( .TRUE., startTime, nIter0, myThid )
       ENDIF
      ENDIF
C-    update also CG2D matrix (and preconditioner)
      IF ( nonlinFreeSurf.GT.2 ) THEN
        CALL UPDATE_CG2D( startTime, nIter0, myThid )
      ENDIF
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INTEGR_CONTINUITY',myThid)
#endif
C--   Integrate continuity vertically for vertical velocity
      CALL INTEGR_CONTINUITY( uVel, vVel,
     I                        startTime, nIter0, myThid )

#ifdef NONLIN_FRSURF
      IF ( select_rStar.NE.0 ) THEN
#ifndef DISABLE_RSTAR_CODE
C--   r* : compute the future level thickness according to etaH(n+1)
          CALL CALC_R_STAR(etaH, startTime, nIter0, myThid )
#endif
      ELSEIF ( nonlinFreeSurf.GT.0 .AND. selectSigmaCoord.EQ.0 ) THEN
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
