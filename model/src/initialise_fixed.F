C $Header: /u/gcmpack/MITgcm/model/src/initialise_fixed.F,v 1.3 2000/09/11 23:05:56 heimbach Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INITIALISE_FIXED(myThid)
C     /==========================================================\
C     | SUBROUTINE INITIALISE_FIXED                              |
C     | o Routine for setting fixed model arrays such as         |
C     |   topography, grid, solver matrices, etc.                |
C     |==========================================================|
C     | INITIALISE_FIXED is invoked at the start of the model to |
C     | set fixed model arrays. It reads data from an input file |
C     | and from various binary files.                           |
C     | Each thread invokes an instance of this routine as does  |
C     | each process in a multi-process parallel environment like|
C     | MPI.                                                     |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
#ifdef ALLOW_OBCS
      INTEGER bi,bj,K
#endif

C--   Set model parameters.
C     Parameters are set to defaults and then updates are read from
C     an input file called data.
      CALL INI_PARMS( myThid )
      _BARRIER

#ifdef ALLOW_OBCS
C--   Open boundaries positions
      CALL INI_OBCS( myThid )
#endif
      _BARRIER

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

C--   Set coriolis operators
      CALL INI_CORI( myThid )

#ifdef INCLUDE_LAT_CIRC_FFT_FILTER_CODE
C--   Latitude circle filter initialisation
      CALL FILTER_LATCIRC_FFT_INIT(myThid)
      _BARRIER
#endif

C--   Set laplace operators for use in 2D conjugate gradient solver.
      CALL INI_CG2D( myThid )

#ifdef ALLOW_NONHYDROSTATIC
C--   Set laplace operators for use in 3D conjugate gradient solver.
      CALL INI_CG3D( myThid )
#endif

C--   Configure packages
      CALL PACKAGES_BOOT( myThid )

C--   Read configuration parameters for packages
      CALL PACKAGES_READPARMS( myThid )

C--   Check dependances between packages
      CALL PACKAGES_CHECK( myThid )

C--   Finally summarise the model cofiguration
      CALL CONFIG_SUMMARY( myThid )

      END
