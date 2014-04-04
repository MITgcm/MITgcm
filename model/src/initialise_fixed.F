C $Header: /u/gcmpack/MITgcm/model/src/initialise_fixed.F,v 1.56 2014/04/04 20:39:25 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INITIALISE_FIXED
C     !INTERFACE:
      SUBROUTINE INITIALISE_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INITIALISE_FIXED
C     | o Routine for setting fixed model arrays such as
C     |   topography, grid, solver matrices, etc.
C     *==========================================================*
C     | INITIALISE_FIXED is invoked at the start of the model to
C     | set fixed model arrays. It reads data from an input file
C     | and from various binary files.
C     | Each thread invokes an instance of this routine as does
C     | each process in a multi-process parallel environment like
C     | MPI.
C     *==========================================================*
C     \ev

C     !CALLING SEQUENCE:
C     INITIALISE_FIXED
C      |
C      |-- INI_PARMS
C      |
C      |-- PACKAGES_BOOT
C      |
C      |-- PACKAGES_READPARMS
C      |  |- ${PKG}_READPARMS
C      |
C      |-- SET_PARMS
C      |
C      |-- INI_MODEL_IO
C      |  |- MNC_INIT + MNC_CW_INIT
C      |  |- MON_INIT
C      |
C      |-- INI_GRID
C      |
C      |-- LOAD_REF_FILES
C      |
C      |-- INI_EOS
C      |
C      |-- SET_REF_STATE
C      |
C      |-- SET_GRID_FACTORS
C      |
C      |-- INI_DEPTHS
C      |
C      |-- INI_MASKS_ETC
C      |
C      |-- PACKAGES_INIT_FIXED
C      |  |- ${PKG}_INIT_FIXED
C      |
C      |-- INI_GLOBAL_DOMAIN
C      |
C      |-- INI_LINEAR_PHISURF
C      |
C      |-- INI_CORI
C      |
C      |-- INI_CG2D
C      |
C      |-- INI_CG3D
C      |
C      |-- CONFIG_SUMMARY
C      |
C      |-- PACKAGES_CHECK
C      |  |- ${PKG}_CHECK
C      |
C      |-- CONFIG_CHECK
C      |
C      |-- WRITE_GRID
C      |
C      |-- CPL_EXCH_CONFIGS

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('INITIALISE_FIXED',myThid)
#endif

C-    here is a way to deliberately break the barrier syncronization
C     by adding a BARRIER on thread XXX only ; Used to test BAR_CHECK.
c     IF ( myThid.EQ.XXX ) THEN
c       _BARRIER
c       CALL SYSTEM('sleep 1')
c     ENDIF
C--   Check barrier synchronization:
      CALL BAR_CHECK( 2, myThid )

C--   Set model parameters.
C     Parameters are set to defaults and then updates are read from
C     an input file called data.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_PARMS',myThid)
#endif
      CALL INI_PARMS( myThid )

C--   Configure packages
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('PACKAGES_BOOT',myThid)
#endif
      CALL PACKAGES_BOOT( myThid )

C--   Read configuration parameters for packages
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('PACKAGES_READPARMS',myThid)
#endif
      CALL PACKAGES_READPARMS( myThid )

C--   Set (or reset) parameters (at this point, know which packages are used);
C     after this call, main model parameters are not supposed to be modified.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('SET_PARMS',myThid)
#endif
      CALL SET_PARMS( myThid )

C--   Write units/set precision/etc for I/O (pickup, MDS/RW, MNC, MONITOR):
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_MODEL_IO',myThid)
#endif
      CALL INI_MODEL_IO( myThid )

C--   Set model grid.
C     Variables defining model grid spacing are defined.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_GRID',myThid)
#endif
#ifdef ALLOW_OPENAD
      CALL OpenAD_INI_GRID( myThid )
#else
      CALL INI_GRID( myThid )
#endif

C--   Load reference profiles from files
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('LOAD_REF_FILES',myThid)
#endif
      CALL LOAD_REF_FILES( myThid )

C--   Set equation of state parameters.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_EOS',myThid)
#endif
      CALL INI_EOS( myThid )

C--   Set reference state (vertical reference profiles)
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('SET_REF_STATE',myThid)
#endif
      CALL SET_REF_STATE( myThid )

C--   Set remaining grid factors
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('SET_GRID_FACTORS',myThid)
#endif
      CALL SET_GRID_FACTORS( myThid )

C--   Initialise map of depths
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_DEPTHS',myThid)
#endif
#ifdef ALLOW_OPENAD
      CALL OpenAD_INI_DEPTHS( myThid )
#else
      CALL INI_DEPTHS( myThid )
#endif

C--   Derive masks, lopping factors and recipricols of quantities.
C     Volumes and areas are set according to grid and depth map.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_MASKS_ETC',myThid)
#endif
#ifdef ALLOW_OPENAD
      CALL OpenAD_INI_MASKS_ETC( myThid )
#else
      CALL INI_MASKS_ETC( myThid )
#endif

C--   Synchronize all threads after setting masks and before pkgs init.
      _BARRIER

C--   Call fixed data initialization phase of packages
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('PACKAGES_INIT_FIXED',myThid)
#endif
      CALL PACKAGES_INIT_FIXED( myThid )

C--   Set some domain-dependent (global) constant (e.g., globalArea)
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_GLOBAL_DOMAIN',myThid)
#endif
      CALL INI_GLOBAL_DOMAIN( myThid )

C--   Set Bo_surf => define the Linear Relation: Phi_surf(eta)
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_LINEAR_PHISURF',myThid)
#endif
#ifdef ALLOW_OPENAD
      CALL OpenAD_INI_LINEAR_PHISURF( myThid )
#else
      CALL INI_LINEAR_PHISURF( myThid )
#endif

C--   Set coriolis operators
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_CORI',myThid)
#endif
#ifdef ALLOW_OPENAD
      CALL OpenAD_INI_CORI( myThid )
#else
      CALL INI_CORI( myThid )
#endif

C--   Set laplace operators for use in 2D conjugate gradient solver.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_CG2D',myThid)
#endif
#ifdef ALLOW_OPENAD
      CALL OpenAD_INI_CG2D( myThid )
#else
      CALL INI_CG2D( myThid )
#endif

#ifdef ALLOW_NONHYDROSTATIC
C--   Set laplace operators for use in 3D conjugate gradient solver.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_CG3D',myThid)
#endif
      CALL INI_CG3D( myThid )
#endif

C--   Finally summarise the model configuration
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('CONFIG_SUMMARY',myThid)
#endif
      CALL CONFIG_SUMMARY( myThid )

C--   Check packages configuration (& print summary)
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('PACKAGES_CHECK',myThid)
#endif
      CALL PACKAGES_CHECK( myThid )

C--   Check parameters and model configuration
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('CONFIG_CHECK',myThid)
#endif
      CALL CONFIG_CHECK( myThid )

C--   Write grid data and geometry arrays
      IF ( debugLevel.GE.debLevA .OR. startTime.EQ.baseTime ) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('WRITE_GRID',myThid)
#endif
        CALL WRITE_GRID( myThid )
      ENDIF

#ifdef COMPONENT_MODULE
C--   Post component-model configuration information to coupler
C     and get config. info for other component(s).
      IF ( useCoupler ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('CPL_EXCH_CONFIGS',myThid)
# endif
        CALL CPL_EXCH_CONFIGS( myThid )
      ENDIF
#endif /* COMPONENT_MODULE */

C--   Check barrier synchronization:
      CALL BAR_CHECK( 3, myThid )

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('INITIALISE_FIXED',myThid)
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
