#include "PROFILES_OPTIONS.h"

C     ==================================================================
C
C     active_files_ad.F: Routines to handle the I/O of the active file
C                        for the adjoint calculations. All files are
C                        direct access files.
C
C     Routines
C
C     o  adactive_read_profile      - Adjoint of active_read_profile
C     o  adactive_write_profile     - Adjoint of active_write_profile
C
C        changed: gforget@ocean.mit.edu 23-Mar-2006
C
C     ==================================================================

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: ADACTIVE_READ_PROFILE

C !INTERFACE:
      SUBROUTINE ADACTIVE_READ_PROFILE(
     I                              active_num_file,
     I                              nadactive_var,
     I                              adactive_var,
     I                              active_varnum,
     I                              irec,
     I                              lAdInit,
     I                              myIter,
     I                              bi,
     I                              bj,
     I                              myThid
     &                            )

C     !DESCRIPTION:
C     Adjoint of active_read_profile.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file: filename
C     nadactive_var:   integer size of adactive_var
C     adactive_var:    array
C     irec:            record number
C     myIter:          number of optimization iteration (default: 0)
C     myThid:          my thread ID number
C     doglobalread:    flag for global or local read/write
C                      (default: .false.)
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      INTEGER nadactive_var,active_num_file
      _RL     adactive_var(nadactive_var)
      INTEGER irec,active_varnum
      INTEGER myIter
      INTEGER bi,bj,myThid
      LOGICAL lAdInit
CEOP

#ifdef ALLOW_PROFILES

      CALL ACTIVE_READ_PROFILE_RL( fidadjoint(active_num_file,bi,bj),
     &     active_num_file,
     &     nadactive_var, adactive_var, active_varnum, lAdInit,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     REVERSE_SIMULATION, myIter,bi,bj, myThid)

#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: ADACTIVE_WRITE_PROFILE

C !INTERFACE:
      SUBROUTINE ADACTIVE_WRITE_PROFILE(
     I                              active_num_file,
     I                              nadactive_var,
     I                              adactive_var,
     I                              active_varnum,
     I                              irec,
     I                              myIter,
     I                              bi,
     I                              bj,
     I                              myThid,
     I                              dummy
     &                            )

C     !DESCRIPTION:
C     Adjoint of active_write_profile.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file: filename
C     nadactive_var:   integer size of adactive_var
C     adactive_var:    array
C     irec:            record number
C     myIter:          number of optimization iteration (default: 0)
C     myThid:          my thread ID number
C     doglobalread:    flag for global or local read/write
C                      (default: .false.)
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      INTEGER nadactive_var,active_num_file
      _RL     adactive_var(nadactive_var)
      INTEGER irec,active_varnum
      INTEGER myIter
      INTEGER bi,bj,myThid
      _RL     dummy
CEOP

#ifdef ALLOW_PROFILES

      CALL ACTIVE_WRITE_PROFILE_RL( fidadjoint(active_num_file,bi,bj),
     &     active_num_file,
     &     nadactive_var, adactive_var, active_varnum,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     REVERSE_SIMULATION, myIter,bi,bj, myThid)

#endif

      RETURN
      END
