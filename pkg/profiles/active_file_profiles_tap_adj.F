#include "PROFILES_OPTIONS.h"

C     ==================================================================
C
C     active_files_tap_adj.F: Routines to handle the I/O of the active file
C                        for the adjoint calculations with Tapenade.
C                        All files are direct access files.
C
C     Routines
C     o  active_read_profile_b      - Adjoint of active_read_profile
C     o  active_write_profile_b     - Adjoint of active_write_profile
C
C        changed: shreyasg911@gmail.com 26-Mar-2023
C
C     ==================================================================

      SUBROUTINE ACTIVE_READ_PROFILE_B(
     I                              active_num_file,
     I                              nadactive_var,
     I                              active_var,
     I                              adactive_var,
     I                              active_varnum,
     I                              irec,
     I                              lAdInit,
     I                              myIter,
     I                              bi,
     I                              bj,
     I                              myThid,
     I                              dummy,
     I                              dummyb
     &                            )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE active_read_profile_b
C     ==================================================================
C     o Adjoint of active_read_profile.
C     ==================================================================
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

C     == routine arguments ==
C     active_var_file: filename
C     nadactive_var:   integer size of adactive_var
C     adactive_var:    array
C     irec:            record number
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
C     myIter:          number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
      INTEGER active_num_file, nadactive_var
      _RL     active_var(nadactive_var), adactive_var(nadactive_var)
      INTEGER active_varnum
      INTEGER irec
      LOGICAL lAdInit
      INTEGER myIter
      INTEGER bi, bj, myThid
      _RL     dummy
      _RL     dummyb
C     == end of interface ==

#ifdef ALLOW_PROFILES

      CALL ACTIVE_READ_PROFILE_RL( fidadjoint(active_num_file,bi,bj) ,
     &     active_num_file,
     &     nadactive_var, adactive_var, active_varnum, lAdInit,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     REVERSE_SIMULATION, myIter,bi,bj, myThid)

#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE ACTIVE_WRITE_PROFILE_B(
     I                              active_num_file,
     I                              nadactive_var,
     I                              active_var,
     I                              adactive_var,
     I                              active_varnum,
     I                              irec,
     I                              myIter,
     I                              bi,
     I                              bj,
     I                              myThid,
     I                              dummy,
     I                              dummyb
     &                            )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE active_write_profile_b
C     ==================================================================
C     o Adjoint of active_write_profile.
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

C     == routine arguments ==
C     active_var_file: filename
C     nadactive_var:   integer size of adactive_var
C     adactive_var:    array
C     irec:            record number
C     myIter:          number of optimization iteration (default: 0)
C     myThid:          thread number for this instance
      INTEGER active_num_file, nadactive_var
      _RL     active_var(nadactive_var), adactive_var(nadactive_var)
      INTEGER active_varnum
      INTEGER irec
      INTEGER myIter
      INTEGER bi, bj, myThid
      _RL     dummy
      _RL     dummyb
C     == end of interface ==

#ifdef ALLOW_PROFILES

      CALL ACTIVE_WRITE_PROFILE_RL( fidadjoint(active_num_file,bi,bj) ,
     &     active_num_file,
     &     nadactive_var, adactive_var, active_varnum,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     REVERSE_SIMULATION, myIter,bi,bj, myThid)

#endif

      RETURN
      END
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
