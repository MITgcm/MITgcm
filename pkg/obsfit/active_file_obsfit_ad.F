#include "OBSFIT_OPTIONS.h"

C--  File active_file_obsfit_ad.F:
C--   Contents
C--   o adactive_read_obs_tile
C--   o adactive_write_obs_tile
C--   o adactive_read_obs_glob
C--   o adactive_write_obs_glob

CBOP
C     !ROUTINE: ADACTIVE_READ_OBS_TILE
C     !INTERFACE:
      SUBROUTINE ADACTIVE_READ_OBS_TILE(
     I                              active_num_file,
     I                              adactive_var,
     I                              irec,
     I                              lAdInit,
     I                              myIter,
     I                              bi,
     I                              bj,
     I                              myThid )

C     !DESCRIPTION:
C     ==================================================================
C     | Adjoint of active_read_obs_tile
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
C     adactive_var:    array
C     irec:            record number
C     myIter:          number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      INTEGER active_num_file
      _RL     adactive_var
      INTEGER irec
      INTEGER myIter
      INTEGER bi, bj, myThid
      LOGICAL lAdInit
CEOP

#ifdef ALLOW_OBSFIT

      CALL active_read_obs_tile_rl( fidadj_obs(active_num_file,bi,bj),
     &     active_num_file,
     &     adactive_var, lAdInit,
     &     irec, sample_ind_glob(active_num_file,irec,bi,bj),
     &     REVERSE_SIMULATION, myIter, bi, bj, myThid)

#endif

      END

C     ==================================================================

CBOP
C     !ROUTINE: ADACTIVE_WRITE_OBS_TILE
C     !INTERFACE:
      SUBROUTINE ADACTIVE_WRITE_OBS_TILE(
     I                                  active_num_file,
     I                                  adactive_var,
     I                                  irec,
     I                                  myIter,
     I                                  bi,
     I                                  bj,
     I                                  myThid,
     I                                  dummy )

C     !DESCRIPTION:
C     ==================================================================
C     | Adjoint of active_write_obs_tile
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
C     adactive_var:    array
C     irec:            record number
C     myIter:          number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      INTEGER active_num_file
      _RL     adactive_var
      INTEGER irec
      INTEGER myIter
      INTEGER bi, bj, myThid
      _RL     dummy
CEOP

#ifdef ALLOW_OBSFIT

      CALL active_write_obs_tile_rl( fidadj_obs(active_num_file,bi,bj),
     &     active_num_file,
     &     adactive_var, 
     &     irec, sample_ind_glob(active_num_file,irec,bi,bj),
     &     REVERSE_SIMULATION, myIter, bi, bj, myThid)

#endif

      END

C     ==================================================================


CBOP
C     !ROUTINE: ADACTIVE_READ_OBS_GLOB
C     !INTERFACE:
      SUBROUTINE ADACTIVE_READ_OBS_GLOB(
     I                              active_num_file,
     I                              adactive_var,
     I                              irec,
     I                              lAdInit,
     I                              myIter,
     I                              myThid )

C     !DESCRIPTION:
C     ==================================================================
C     | SUBROUTINE ADACTIVE_READ_OBS_GLOB
C     | Adjoint of ACTIVE_READ_OBS_GLOB.F
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
C     adactive_var:    array
C     irec:            record number
C     myIter:          number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      INTEGER active_num_file
      _RL     adactive_var
      INTEGER irec
      INTEGER myIter
      INTEGER myThid
      LOGICAL lAdInit
CEOP

#ifdef ALLOW_OBSFIT

      CALL active_read_obs_glob_rl( fidadglobal(active_num_file),
     &     active_num_file,
     &     adactive_var, lAdInit,
     &     irec, obs_ind_glob(active_num_file,irec),
     &     REVERSE_SIMULATION, myIter, myThid)

#endif

      END

C     ==================================================================

CBOP
C     !ROUTINE: ADACTIVE_WRITE_OBS_GLOB
C     !INTERFACE:
      SUBROUTINE ADACTIVE_WRITE_OBS_GLOB(
     I                                  active_num_file,
     I                                  adactive_var,
     I                                  irec,
     I                                  myIter,
     I                                  myThid,
     I                                  dummy )

C     !DESCRIPTION:
C     ==================================================================
C     | SUBROUTINE ADACTIVE_WRITE_OBS_GLOB
C     | Adjoint of ACTIVE_WRITE_OBS_GLOB.F
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
C     adactive_var:    array
C     irec:            record number
C     myIter:          number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      INTEGER active_num_file
      _RL     adactive_var
      INTEGER irec
      INTEGER myIter
      INTEGER myThid
      _RL     dummy
CEOP

#ifdef ALLOW_OBSFIT

      CALL active_write_obs_glob_rl( fidadglobal(active_num_file),
     &     active_num_file,
     &     adactive_var,
     &     irec, obs_ind_glob(active_num_file,irec),
     &     REVERSE_SIMULATION, myIter,myThid)

#endif

      END

C     ==================================================================

