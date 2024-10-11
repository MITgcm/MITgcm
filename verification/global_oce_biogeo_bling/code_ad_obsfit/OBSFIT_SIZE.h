CBOP
C     !ROUTINE: OBSFIT_SIZE.h
C     !INTERFACE:
C     #include "OBSFIT_SIZE.h"

C     !DESCRIPTION:
C     *================================================================*
C     | OBSFIT_SIZE.h
C     | o Header file defining "obsfit" arrays sizes
C     *================================================================*
CEOP

C NFILESMAX_OBS      :: maximum number of input files
C NOBSMAX_OBS        :: maximum number of observations per file per tile
C NSAMPLESMAX        :: maximum number of samples per file per tile
C NSAMP_PER_OBS_MAX  :: maximum number of samples per observation 
C NUM_INTERP_PTS_OBS :: number of points used in interpolation for 
C                       model sampling

      INTEGER NFILESMAX_OBS
      PARAMETER ( NFILESMAX_OBS=5 )

      INTEGER NOBSMAX_OBS
      PARAMETER ( NOBSMAX_OBS=11000 )

      INTEGER NSAMPLESMAX
      PARAMETER ( NSAMPLESMAX=11000 )

      INTEGER NSAMP_PER_OBS_MAX
      PARAMETER ( NSAMP_PER_OBS_MAX=2 )

      INTEGER NUM_INTERP_PTS_OBS
      PARAMETER ( NUM_INTERP_PTS_OBS=8 )

