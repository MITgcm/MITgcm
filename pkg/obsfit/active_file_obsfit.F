#include "OBSFIT_OPTIONS.h"
C--  File active_file_control_obs_tile_rl.F:
C--   Contents
C--   o active_read_obs_tile_rl
C--   o active_write_obs_tile_rl
C--   o active_read_obs_glob_rl
C--   o active_write_obs_glob_rl

CBOP
C     !ROUTINE: ACTIVE_READ_OBS_TILE

C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_OBS_TILE(
     I                               active_num_file,
     O                               active_var,
     I                               irec,
     I                               lAdInit,
     I                               myOptimIter,
     I                               bi,
     I                               bj,
     I                               myThid,
     I                               dummy )
C     !DESCRIPTION: 
C     ==================================================================
C     | Read an active record ("sample-equivalent") from an ObsFit 
C     | .equi. tiled file (can be netcdf or binary)  
C     ==================================================================

C     !USES: 
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif

C     !INPUT PARAMETERS:
C     active_num_file: file number
C     active_var:      array
C     irec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      INTEGER  active_num_file
      _RL      active_var
      INTEGER  irec
      INTEGER  myOptimIter
      INTEGER  bi, bj, myThid
      LOGICAL  lAdInit
      _RL      dummy
CEOP

#ifdef ALLOW_OBSFIT

      CALL ACTIVE_READ_OBS_TILE_RL( fidfwd_obs(active_num_file,bi,bj),
     &     active_num_file,
     &     active_var, lAdInit,
     &     irec, sample_ind_glob(active_num_file,irec,bi,bj),
     &     FORWARD_SIMULATION, myOptimIter, bi, bj, myThid)

#endif

      RETURN
      END

C     ==================================================================

CBOP
C     !ROUTINE: ACTIVE_WRITE_OBS_TILE

C     !INTERFACE:
      SUBROUTINE ACTIVE_WRITE_OBS_TILE(
     I                           active_num_file,
     I                           active_var,
     I                           irec,
     I                           myOptimIter,
     I                           bi,
     I                           bj,
     I                           myThid,
     I                           dummy )

C     !DESCRIPTION: 
C     ==================================================================
C     | Write to a file a sampled value during the model run
C     ==================================================================

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif

C     !INPUT PARAMETERS:
C     active_num_file: file number
C     active_var:      array
C     irec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
      INTEGER  active_num_file
      _RL      active_var
      INTEGER  irec
      INTEGER  myOptimIter
      INTEGER  bi, bj, myThid
      _RL      dummy
CEOP

#ifdef ALLOW_OBSFIT

      CALL ACTIVE_WRITE_OBS_TILE_RL( fidfwd_obs(active_num_file,bi,bj),
     &     active_num_file,
     &     active_var, 
     &     irec, sample_ind_glob(active_num_file,irec,bi,bj),
     &     FORWARD_SIMULATION, myOptimIter, bi, bj, myThid)

#endif

      RETURN
      END

C     ==================================================================

CBOP
C     !ROUTINE: ACTIVE_READ_OBS_GLOB

C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_OBS_GLOB(
     I                               active_num_file,
     O                               active_var,
     I                               irec,
     I                               lAdInit,
     I                               myOptimIter,
     I                               myThid,
     I                               dummy )

C     !DESCRIPTION: 
C     ==================================================================
C     | Read an active record ("observation-equivalent") from a
C     | global file
C     ==================================================================

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif

C     !INPUT PARAMETERS:
C     active_num_file: file number
C     active_var:      array
C     irec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      INTEGER  active_num_file
      _RL      active_var
      INTEGER  irec
      INTEGER  myOptimIter
      INTEGER  myThid
      LOGICAL  lAdInit
      _RL      dummy
CEOP

#ifdef ALLOW_OBSFIT

      CALL ACTIVE_READ_OBS_GLOB_RL( 
     &     fidglobal(active_num_file),
     &     active_num_file,
     &     active_var, lAdInit,
     &     irec, obs_ind_glob(active_num_file,irec),
     &     FORWARD_SIMULATION, myOptimIter, myThid)

#endif

      RETURN
      END

C     ==================================================================

CBOP
C     !ROUTINE: ACTIVE_WRITE_OBS_GLOB

C     !INTERFACE:
      SUBROUTINE ACTIVE_WRITE_OBS_GLOB(
     I                           active_num_file,
     I                           active_var,
     I                           irec,
     I                           myOptimIter,
     I                           myThid,
     I                           dummy )

C     !DESCRIPTION: 
C     ==================================================================
C     | Write to a global file a model counterpart to an observation
C     ==================================================================

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif

C     !INPUT PARAMETERS:
C     active_num_file: file number
C     active_var:      array
C     irec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
      INTEGER  active_num_file
      _RL      active_var
      INTEGER  irec
      INTEGER  myOptimIter
      INTEGER  myThid
      _RL      dummy
CEOP

#ifdef ALLOW_OBSFIT

      CALL ACTIVE_WRITE_OBS_GLOB_RL( 
     &     fidglobal(active_num_file),
     &     active_num_file,
     &     active_var, 
     &     irec, obs_ind_glob(active_num_file,irec),
     &     FORWARD_SIMULATION, myOptimIter, myThid)

#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|