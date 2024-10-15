#include "PROFILES_OPTIONS.h"

C     ==================================================================
C     active_file_profiles_g.F: Routines that handle the I/O of
C                               active variables for the tangently linear
C                               calculations, related to netcdf
C                               profiles data files
C
C     Routines
C     o  g_active_read_profile  - Read an active 1 record from file
C                                 fwd-mode only: including a mask
C     o  g_active_write_profile - Write nn active 1D record to file.
C                                 fwd-mode only: including a mask
C
c     changed: gforget@ocean.mit.edu 23-Mar-2006
C     ==================================================================

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: G_ACTIVE_READ_PROFILE(
C     !INTERFACE:
      SUBROUTINE G_ACTIVE_READ_PROFILE(
     I                           active_num_file,
     I                           nactive_var,
     O                           active_var,
     I                           g_active_var,
     I                           active_varnum,
     I                           irec,
     I                           lAdInit,
     I                           myOptimIter,
     I                           bi,
     I                           bj,
     I                           myThid,
     I                           dummy
     &                         )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE g_active_read_profile
C     ==================================================================
C     o Read active 1D records from file for tangent linear simulation
C
C     \ev

C     !USES:
      IMPLICIT NONE

#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

C     !INPUT PARAMETERS:
C     active_var_file: filename
C     nactive_var:     integer size of active_var
C     active_var:      array
C     irec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      INTEGER active_num_file
      INTEGER nactive_var
      INTEGER active_varnum
      INTEGER irec
      INTEGER myOptimIter
      INTEGER bi, bj, myThid
      LOGICAL lAdInit
C     !OUTPUT PARAMETERS:
      _RL     active_var(nactive_var)
      _RL     g_active_var(nactive_var)
      _RL     dummy

#ifdef ALLOW_PROFILES

      CALL ACTIVE_READ_PROFILE_RL( fidforward(active_num_file,bi,bj),
     &     active_num_file,
     &     nactive_var,  active_var, active_varnum, lAdInit,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     FORWARD_SIMULATION, myOptimIter,bi,bj, myThid)

      CALL ACTIVE_READ_PROFILE_RL( fidtangent(active_num_file,bi,bj),
     &     active_num_file,
     &     nactive_var, g_active_var,active_varnum, lAdInit,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     TANGENT_SIMULATION, myOptimIter,bi,bj, myThid)

#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: G_ACTIVE_WRITE_PROFILE(
C     !INTERFACE:
      SUBROUTINE G_ACTIVE_WRITE_PROFILE(
     I                           active_num_file,
     I                           nactive_var,
     I                           active_var,
     I                           g_active_var,
     I                           active_varnum,
     I                           irec,
     I                           myOptimIter,
     I                           bi,
     I                           bj,
     I                           myThid,
     I                           dummy,
     I                           g_dummy
     &                                )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE g_active_write_profile
C     ==================================================================
C     o write active 1D records to file for tangent linear simulation
C
C     \ev

C     !USES:
      IMPLICIT NONE

#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

C     !INPUT PARAMETERS:
C     active_var_file: filename
C     nactive_var:     integer size of active_var
C     active_var:      array
C     irec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
      INTEGER nactive_var,active_num_file
      _RL     active_var(nactive_var)
      _RL     g_active_var(nactive_var)
      INTEGER irec,active_varnum
      INTEGER myOptimIter
      INTEGER bi,bj,myThid
      _RL     dummy
      _RL     g_dummy
C     !OUTPUT PARAMETERS:

#ifdef ALLOW_PROFILES

      CALL ACTIVE_WRITE_PROFILE_RL( fidforward(active_num_file,bi,bj) ,
     &     active_num_file,
     &     nactive_var, active_var, active_varnum,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     FORWARD_SIMULATION, myOptimIter,bi,bj, myThid)

      CALL ACTIVE_WRITE_PROFILE_RL( fidtangent(active_num_file,bi,bj) ,
     &     active_num_file,
     &     nactive_var, g_active_var, active_varnum,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     TANGENT_SIMULATION, myOptimIter,bi,bj, myThid)

#endif

      RETURN
      END
