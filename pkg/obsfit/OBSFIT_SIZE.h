CBOP
C     !ROUTINE: OBSFIT_SIZE.h

C     !INTERFACE:
C     #include "OBSFIT_SIZE.h"

C     !DESCRIPTION:
C     ==================================================================
C     | Header file defining ObsFit arrays sizes
C     ==================================================================
CEOP

C     NFILESMAX_OBS      :: maximum number of input files
C     NOBSMAX_OBS        :: maximum number of obs per file per tile
C     NSAMPLESMAX        :: maximum number of samples per file per tile
C     NSAMP_PER_OBS_MAX  :: maximum number of samples per observation 
C     NUM_INTERP_PTS_OBS :: number of points used in interpolation for 
C                           model sampling

      INTEGER NFILESMAX_OBS
      PARAMETER ( NFILESMAX_OBS=5 )

      INTEGER NOBSMAX_OBS
      PARAMETER ( NOBSMAX_OBS=1000 )

      INTEGER NSAMPLESMAX
      PARAMETER ( NSAMPLESMAX=1000 )

      INTEGER NSAMP_PER_OBS_MAX
      PARAMETER ( NSAMP_PER_OBS_MAX=1 )

      INTEGER NUM_INTERP_PTS_OBS
      PARAMETER ( NUM_INTERP_PTS_OBS=8 )

