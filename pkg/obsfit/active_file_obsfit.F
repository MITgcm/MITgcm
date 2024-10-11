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
C     | SUBROUTINE ACTIVE_READ_OBS_TILE
C     | Read an active record from an obsfit .equi. tiled file
C     | (can be netcdf or binary)  
C     ==================================================================

C !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif

C !INPUT PARAMETERS:
c     active_var_file: filename
c     active_var:      array
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     myThid:          thread number for this instance
c     lAdInit:         initialisation of corresponding adjoint
c                      variable and write to active file
      INTEGER  active_num_file
      _RL      active_var
      INTEGER  irec
      INTEGER  myOptimIter
      INTEGER  bi, bj, myThid
      LOGICAL  lAdInit
      _RL      dummy
CEOP

#ifdef ALLOW_OBSFIT

      CALL active_read_obs_tile_rl( fidfwd_obs(active_num_file,bi,bj),
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
C     | SUBROUTINE ACTIVE_WRITE_OBS_TILE
C     | Write to a file the model-equivalent value to an  
c     | observation sample during the model run
C     ==================================================================

C !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif

C !INPUT PARAMETERS:
c     active_var_file: filename
c     active_var:      array
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     myThid:          thread number for this instance
      INTEGER  active_num_file
      _RL      active_var
      INTEGER  irec
      INTEGER  myOptimIter
      INTEGER  bi, bj, myThid
      _RL      dummy
CEOP

#ifdef ALLOW_OBSFIT

      CALL active_write_obs_tile_rl( fidfwd_obs(active_num_file,bi,bj),
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
C     | SUBROUTINE ACTIVE_READ_OBS_GLOB
C     | Read an active record from an obsfit .equi. tiled file
C     | (can be netcdf or binary)  
C     ==================================================================

C !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif

C !INPUT PARAMETERS:
c     active_var_file: filename
c     active_var:      array
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     myThid:          thread number for this instance
c     lAdInit:         initialisation of corresponding adjoint
c                      variable and write to active file
      INTEGER  active_num_file
      _RL      active_var
      INTEGER  irec
      INTEGER  myOptimIter
      INTEGER  myThid
      LOGICAL  lAdInit
      _RL      dummy
CEOP

#ifdef ALLOW_OBSFIT

      CALL active_read_obs_glob_rl( 
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
C     | SUBROUTINE ACTIVE_WRITE_OBS_GLOB
C     | Write to a file the model-equivalent value to an  
c     | observation sample during the model run
C     ==================================================================

C !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif

C !INPUT PARAMETERS:
c     active_var_file: filename
c     active_var:      array
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     myThid:          thread number for this instance
      INTEGER  active_num_file
      _RL      active_var
      INTEGER  irec
      INTEGER  myOptimIter
      INTEGER  myThid
      _RL      dummy
CEOP

#ifdef ALLOW_OBSFIT

      CALL active_write_obs_glob_rl( 
     &     fidglobal(active_num_file),
     &     active_num_file,
     &     active_var, 
     &     irec, obs_ind_glob(active_num_file,irec),
     &     FORWARD_SIMULATION, myOptimIter, myThid)

#endif

      RETURN
      END

C     ==================================================================


