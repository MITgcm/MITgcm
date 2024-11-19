#include "PROFILES_OPTIONS.h"

C     ==================================================================
C
C     active_file.F: Routines to handle the I/O of the active file for
C                    the adjoint calculations. All files are direct
C                    access files.
C
C     Routines
C
C    o  active_read_profile       - Read  an active 1D record.
C    o  active_write_profile      - Write an active 1D record.
C
C
C        changed: gforget@ocean.mit.edu 23-Mar-2006
C
C     ==================================================================

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: ACTIVE_READ_PROFILE

C !INTERFACE:
      SUBROUTINE ACTIVE_READ_PROFILE(
     I                           active_num_file,
     I                           nactive_var,
     O                           active_var,
     I                           active_varnum,
     I                           irec,
     I                           lAdInit,
     I                           myOptimIter,
     I                           bi,
     I                           bj,
     I                           myThid,
     I                           dummy
     &                         )

C     !DESCRIPTION:
C     Read an active 1D record from an profile data file.

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
C     nactive_var:     integer size of active_var
C     active_var:      array
C     active_varname:  name for the quantity to save
C     irec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          my thread ID number
C     lAdInit:         initialisation of corresponding adjoint
C                      variable and write to active file
      INTEGER  nactive_var,active_num_file
      _RL      active_var(nactive_var)
      INTEGER  irec,active_varnum
      INTEGER  myOptimIter
      INTEGER  bi,bj,myThid
      LOGICAL  lAdInit
      _RL      dummy
CEOP

#ifdef ALLOW_PROFILES

      CALL ACTIVE_READ_PROFILE_RL( fidforward(active_num_file,bi,bj),
     &     active_num_file,
     &     nactive_var, active_var, active_varnum, lAdInit,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     FORWARD_SIMULATION, myOptimIter,bi,bj, myThid)

#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: ACTIVE_WRITE_PROFILE

C !INTERFACE:
      SUBROUTINE ACTIVE_WRITE_PROFILE(
     I                           active_num_file,
     I                           nactive_var,
     I                           active_var,
     I                           active_varnum,
     I                           irec,
     I                           myOptimIter,
     I                           bi,
     I                           bj,
     I                           myThid,
     I                           dummy
     &                         )

C     !DESCRIPTION:
C     Write an active 1D record from file.

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
C     nactive_var:     integer size of active_var
C     active_var:      array
C     active_varname:  name for the quantity to save
C     irec:            record number
C     myOptimIter:     number of optimization iteration (default: 0)
C     myThid:          my thread ID number
      INTEGER  nactive_var,active_num_file, active_varnum
      _RL      active_var(nactive_var)
      INTEGER  irec
      INTEGER  myOptimIter
      INTEGER  bi,bj,myThid
      _RL      dummy
CEOP

#ifdef ALLOW_PROFILES

      CALL ACTIVE_WRITE_PROFILE_RL( fidforward(active_num_file,bi,bj),
     &     active_num_file,
     &     nactive_var, active_var, active_varnum,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     FORWARD_SIMULATION, myOptimIter,bi,bj, myThid)

#endif

      RETURN
      END
