C $Header: /u/gcmpack/MITgcm/model/src/initialise_fixed.F,v 1.52 2011/06/08 01:45:24 jmc Exp $
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
      CALL INI_PARMS( myThid )

C--   Configure packages
      CALL PACKAGES_BOOT( myThid )

C--   Read configuration parameters for packages
      CALL PACKAGES_READPARMS( myThid )

C--   Set (or reset) parameters (at this point, know which packages are used);
C     after this call, main model parameters are not supposed to be modified.
      CALL SET_PARMS( myThid )

C--   Write units/set precision/etc for I/O (pickup, MDS/RW, MNC, MONITOR):
      CALL INI_MODEL_IO( myThid )

C--   Set model grid.
C     Variables defining model grid spacing are defined.
      CALL INI_GRID( myThid )

C--   Load reference profiles from files
      CALL LOAD_REF_FILES( myThid )

C--   Set equation of state parameters.
      CALL INI_EOS( myThid )

C--   Set reference state (vertical reference profiles)
      CALL SET_REF_STATE( myThid )

C--   Set remaining grid factors
      CALL SET_GRID_FACTORS( myThid )

C--   Initialise map of depths
      CALL INI_DEPTHS( myThid )

C--   Derive masks, lopping factors and recipricols of quantities.
C     Volumes and areas are set according to grid and depth map.
      CALL INI_MASKS_ETC( myThid )

C--   Synchronize all threads after setting masks and before pkgs init.
      _BARRIER

C--   Call fixed data initialization phase of packages
      CALL PACKAGES_INIT_FIXED( myThid )

C--   Set Bo_surf => define the Linear Relation: Phi_surf(eta)
      CALL INI_LINEAR_PHISURF( myThid )

C--   Set coriolis operators
      CALL INI_CORI( myThid )

C--   Set laplace operators for use in 2D conjugate gradient solver.
      CALL INI_CG2D( myThid )

#ifdef ALLOW_NONHYDROSTATIC
C--   Set laplace operators for use in 3D conjugate gradient solver.
      CALL INI_CG3D( myThid )
#endif

C--   Finally summarise the model configuration
      CALL CONFIG_SUMMARY( myThid )

C--   Check packages configuration (& print summary)
      CALL PACKAGES_CHECK( myThid )

C--   Check parameters and model configuration
      CALL CONFIG_CHECK( myThid )

C--   Write grid data and geometry arrays
      IF ( debugLevel.GE.debLevA .OR.
     &     startTime.EQ.baseTime ) CALL WRITE_GRID( myThid )

#ifdef COMPONENT_MODULE
C--   Post component-model configuration information to coupler
C     and get config. info for other component(s).
      IF ( useCoupler ) CALL CPL_EXCH_CONFIGS( myThid )
#endif

C--   Check barrier synchronization:
      CALL BAR_CHECK( 3, myThid )

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('INITIALISE_FIXED',myThid)
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
