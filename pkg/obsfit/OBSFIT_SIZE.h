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
C     NOBSMAX_OBS        :: maximum number of obs per file
C     NSAMPLES_MAX_GLO   :: maximum number of samples per file
C     NSAMP_PER_TILE_MAX :: maximum number of samples per file per tile
C     NSAMP_PER_OBS_MAX  :: maximum number of samples per observation
C     NUM_INTERP_PTS_OBS :: number of points used in interpolation for
C                           model sampling

C Max number of input files listed in data.obsfit
      INTEGER NFILESMAX_OBS
      PARAMETER ( NFILESMAX_OBS=2 )

C Max number of "valid" observations (i.e. with date within
C the model run)
      INTEGER NOBSMAX_OBS
      PARAMETER ( NOBSMAX_OBS=2000 )

C Max total number of samples in the file
C (not only "valid" samples)
      INTEGER NSAMPLES_MAX_GLO
      PARAMETER ( NSAMPLES_MAX_GLO=2000 )

C Max number of "valid" samples in each tile
      INTEGER NSAMP_PER_TILE_MAX
      PARAMETER ( NSAMP_PER_TILE_MAX=2000 )

C Max number of samples per observation. For point observations 
C this is 1. For spatial averages or integrals, set this to the 
C maximum number of sampled points contributing to a single observation. 
      INTEGER NSAMP_PER_OBS_MAX
      PARAMETER ( NSAMP_PER_OBS_MAX=1 )

C This number is not to be customized
      INTEGER NUM_INTERP_PTS_OBS
      PARAMETER ( NUM_INTERP_PTS_OBS=8 )

