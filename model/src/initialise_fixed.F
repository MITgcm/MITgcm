C $Header: /u/gcmpack/MITgcm/model/src/initialise_fixed.F,v 1.9 2002/01/31 14:52:28 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INITIALISE_FIXED
C     !INTERFACE:
      SUBROUTINE INITIALISE_FIXED(myThid)

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
C      |-- MON_INIT
C      |
C      |-- INI_GRID
C      |
C      |-- INI_DEPTHS
C      |
C      |-- INI_MASKS_ETC
C      |
C      |-- INI_LINEAR_PHSURF
C      |
C      |-- INI_CORI
C      |
C      |-- PACKAGES_BOOT
C      |
C      |-- PACKAGES_READPARMS
C      |
C      |-- PACKAGES_CHECK
C      |
C      |-- PACKAGES_INIT_FIXED
C      |
C      |-- ZONAL_FILT_INIT
C      |
C      |-- INI_CG2D
C      |
C      |-- INI_CG3D
C      |
C      |-- CONFIG_CHECK
C      |
C      |-- CONFIG_SUMMARY

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

C--   Set model parameters.
C     Parameters are set to defaults and then updates are read from
C     an input file called data.
      CALL INI_PARMS( myThid )
      _BARRIER

#ifndef EXCLUDE_MONITOR
C--   Initialise MONITOR I/O streams so we can report config. info
      CALL MON_INIT( myThid )
      _BARRIER
#endif


C--   Set model grid.
C     Variables defining model grid spacing are defined.
      CALL INI_GRID( myThid )
      _BARRIER

C--   Initialise map of depths
      CALL INI_DEPTHS( myThid )
      _BARRIER

C--   Derive masks, lopping factors and recipricols of quantities.
C     Volumes and areas are set according to grid and depth map.
      CALL INI_MASKS_ETC( myThid )
      _BARRIER

C--   Set Bo_surf => define the Linear Relation: Phi_surf(eta)
      CALL INI_LINEAR_PHISURF( myThid )

C--   Set coriolis operators
      CALL INI_CORI( myThid )

C--   Configure packages
      CALL PACKAGES_BOOT( myThid )

C--   Read configuration parameters for packages
      CALL PACKAGES_READPARMS( myThid )

C--   Check dependances between packages
      CALL PACKAGES_CHECK( myThid )

C--   Call fixed data initialization phase of packages
      CALL PACKAGES_INIT_FIXED( myThid )

#ifdef ALLOW_ZONAL_FILT
C--   Latitude circle filter initialisation
      CALL ZONAL_FILT_INIT(myThid)
      _BARRIER
#endif

C--   Set laplace operators for use in 2D conjugate gradient solver.
      CALL INI_CG2D( myThid )

#ifdef ALLOW_NONHYDROSTATIC
C--   Set laplace operators for use in 3D conjugate gradient solver.
      CALL INI_CG3D( myThid )
#endif

C--   Check parameters and model cofiguration
      CALL CONFIG_CHECK( myThid )

C--   Finally summarise the model cofiguration
      CALL CONFIG_SUMMARY( myThid )

      END
