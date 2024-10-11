#include "OBSFIT_OPTIONS.h"

C--  File active_file_obsfit_g.F:
C--   Contents
C--   o g_active_read_obs_tile
C--   o g_active_write_obs_tile
C--   o g_active_read_obs_glob
C--   o g_active_write_obs_glob

CBOP
C     !ROUTINE: G_ACTIVE_READ_OBS_TILE
C     !INTERFACE:
      SUBROUTINE G_ACTIVE_READ_OBS_TILE(
     I                                 active_num_file,
     O                                 active_var,
     I                                 g_active_var,
     I                                 irec,
     I                                 lAdInit,
     I                                 myOptimIter,
     I                                 bi,
     I                                 bj,
     I                                 myThid,
     I                                 dummy)

C     !DESCRIPTION: 
C     ==================================================================
C     | SUBROUTINE G_ACTIVE_READ_OBS_TILE
C     | Read an active record from an obsfit .equi. tiled file
C     | for tangent linear calculations
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
C     active_num_file: file number
C     active_var:      array
C     irec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      _RL     active_var
      _RL     g_active_var
      INTEGER irec, active_num_file
      INTEGER myOptimIter
      INTEGER bi, bj, myThid
      LOGICAL lAdInit
      _RL     dummy
CEOP

#ifdef ALLOW_OBSFIT

      CALL active_read_obs_tile_rl( fidfwd_obs(active_num_file,bi,bj),
     &     active_num_file,
     &     active_var, lAdInit,
     &     irec, sample_ind_glob(active_num_file,irec,bi,bj),
     &     TANGENT_SIMULATION, myOptimIter, bi, bj, myThid)

      CALL active_read_obs_tile_rl( fidtan_obs(active_num_file,bi,bj),
     &     active_num_file,
     &     g_active_var, lAdInit,
     &     irec, sample_ind_glob(active_num_file,irec,bi,bj),
     &     TANGENT_SIMULATION, myOptimIter, bi, bj, myThid)

#endif

      RETURN
      END


C     ==================================================================

CBOP
C     !ROUTINE: G_ACTIVE_WRITE_OBS_TILE
C     !INTERFACE:
      SUBROUTINE G_ACTIVE_WRITE_OBS_TILE(
     I                           active_num_file,
     I                           active_var,
     I                           g_active_var,
     I                           irec,
     I                           myOptimIter,
     I                           bi,
     I                           bj,
     I                           myThid,
     I                           dummy,
     I                           g_dummy )

C     !DESCRIPTION: 
C     ==================================================================
C     | SUBROUTINE G_ACTIVE_READ_OBS_TILE
C     | Read an active record from an obsfit .equi. tiled file
C     | for tangent linear calculations
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
C     active_num_file: file number
C     active_var:      array
C     irec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
      INTEGER active_num_file
      _RL     active_var
      _RL     g_active_var
      INTEGER irec
      INTEGER myOptimIter
      INTEGER bi, bj, myThid
      _RL     dummy
      _RL     g_dummy
CEOP

#ifdef ALLOW_OBSFIT

      CALL active_write_obs_tile_rl( fidfwd_obs(active_num_file,bi,bj),
     &     active_num_file,
     &     active_var,
     &     irec, sample_ind_glob(active_num_file,irec,bi,bj),
     &     TANGENT_SIMULATION, myOptimIter, bi, bj, myThid)

      CALL active_write_obs_tile_rl( fidtan_obs(active_num_file,bi,bj),
     &     active_num_file,
     &     g_active_var, 
     &     irec, sample_ind_glob(active_num_file,irec,bi,bj),
     &     TANGENT_SIMULATION, myOptimIter, bi, bj, myThid)

#endif

      RETURN
      END

C     ==================================================================


CBOP
C     !ROUTINE: G_ACTIVE_READ_OBS_GLOB
C     !INTERFACE:
      SUBROUTINE G_ACTIVE_READ_OBS_GLOB(
     I                                 active_num_file,
     O                                 active_var,
     I                                 irec,
     I                                 lAdInit,
     I                                 myOptimIter,
     I                                 myThid,
     I                                 dummy,
     I                                 g_active_var )

C     !DESCRIPTION: 
C     ==================================================================
C     | SUBROUTINE G_ACTIVE_READ_OBS_GLOB
C     | Read an active record from an obsfit .equi. tiled file
C     | for tangent linear calculations
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
C     active_num_file: file number
C     active_var:      array
C     irec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      _RL     active_var
      _RL     g_active_var
      INTEGER irec, active_num_file
      INTEGER myOptimIter
      INTEGER myThid
      LOGICAL lAdInit
      _RL     dummy
CEOP

#ifdef ALLOW_OBSFIT

      CALL active_read_obs_glob_rl( fidglobal(active_num_file),
     &     active_num_file,
     &     active_var,  lAdInit,
     &     irec, obs_ind_glob(active_num_file,irec),
     &     TANGENT_SIMULATION, myOptimIter, myThid)

      CALL active_read_obs_glob_rl( fidtanglobal(active_num_file),
     &     active_num_file,
     &     g_active_var, lAdInit,
     &     irec, obs_ind_glob(active_num_file,irec),
     &     TANGENT_SIMULATION, myOptimIter, myThid)

#endif

      RETURN
      END


C     ==================================================================

CBOP
C     !ROUTINE: G_ACTIVE_WRITE_OBS_GLOB
C     !INTERFACE:
      SUBROUTINE G_ACTIVE_WRITE_OBS_GLOB(
     I                           active_num_file,
     I                           active_var,
     I                           irec,
     I                           myOptimIter,
     I                           myThid,
     I                           dummy,
     I                           g_active_var,
     I                           g_dummy )

C     !DESCRIPTION: 
C     ==================================================================
C     | SUBROUTINE G_ACTIVE_READ_OBS_GLOB
C     | Read an active record from an obsfit .equi. tiled file
C     | for tangent linear calculations
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
C     active_num_file: file number
C     active_var:      array
C     irec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
      integer active_num_file
      _RL     active_var
      _RL     g_active_var
      integer irec
      integer myOptimIter
      integer myThid
      _RL     dummy
      _RL     g_dummy
CEOP

#ifdef ALLOW_OBSFIT

      CALL active_write_obs_glob_rl( fidglobal(active_num_file),
     &     active_num_file,
     &     active_var,
     &     irec, obs_ind_glob(active_num_file,irec),
     &     TANGENT_SIMULATION, myOptimIter, myThid)

      CALL active_write_obs_glob_rl( fidtanglobal(active_num_file),
     &     active_num_file,
     &     g_active_var,
     &     irec, obs_ind_glob(active_num_file,irec),
     &     TANGENT_SIMULATION, myOptimIter, myThid)

#endif

      RETURN
      END
C     ==================================================================


