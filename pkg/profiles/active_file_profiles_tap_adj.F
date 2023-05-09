#include "PROFILES_OPTIONS.h"

c     ==================================================================
c
c     active_files_tap_adj.F: Routines to handle the I/O of the active file
c                        for the adjoint calculations with Tapenade.
c                        All files are direct access files.
c
c     Routines
c     o  active_read_profile_b      - Adjoint of active_read_profile
c     o  active_write_profile_b     - Adjoint of active_write_profile
c
c        changed: shreyasg911@gmail.com 26-Mar-2023
c
c     ==================================================================

      subroutine active_read_profile_b(
     I                              active_num_file,
     I                              nadactive_var,
     I                              active_var,
     I                              adactive_var,
     I                              active_varnum,
     I                              irec,
     I                              lAdInit,
     I                              myiter,
     I                              bi,
     I                              bj,
     I                              mythid,
     I                              dummy,
     I                              dummyb
     &                            )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE active_read_profile_b
c     ==================================================================
c     o Adjoint of active_read_profile.
c     ==================================================================
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

c     == routine arguments ==
c     active_var_file: filename
c     nadactive_var:   integer size of adactive_var
c     adactive_var:    array
c     irec:            record number
c     lAdInit:         initialisation of corresponding adjoint
c                      variable and write to active file
c     myIter:          number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
      integer active_num_file, nadactive_var
      _RL    active_var(nadactive_var), adactive_var(nadactive_var)
      integer active_varnum
      integer irec
      logical lAdInit
      integer myiter
      integer  bi, bj, mythid
      _RL      dummy
      _RL      dummyb
c     == end of interface ==

#ifdef ALLOW_PROFILES

      call active_read_profile_rl( fidadjoint(active_num_file,bi,bj) ,
     &     active_num_file,
     &     nadactive_var, adactive_var, active_varnum, lAdInit,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     REVERSE_SIMULATION, myiter,bi,bj, mythid)

#endif

      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      subroutine active_write_profile_b(
     I                              active_num_file,
     I                              nadactive_var,
     I                              active_var,
     I                              adactive_var,
     I                              active_varnum,
     I                              irec,
     I                              myiter,
     I                              bi,
     I                              bj,
     I                              mythid,
     I                              dummy,
     I                              dummyb
     &                            )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE active_write_profile_b
c     ==================================================================
c     o Adjoint of active_write_profile.
c     ==================================================================

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

c     == routine arguments ==
c     active_var_file: filename
c     nadactive_var:   integer size of adactive_var
c     adactive_var:    array
c     irec:            record number
c     myIter:          number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
      integer active_num_file, nadactive_var
      _RL    active_var(nadactive_var), adactive_var(nadactive_var)
      integer active_varnum
      integer irec
      integer myiter
      integer  bi, bj, mythid
      _RL     dummy
      _RL     dummyb
c     == end of interface ==

#ifdef ALLOW_PROFILES

      call active_write_profile_rl( fidadjoint(active_num_file,bi,bj) ,
     &     active_num_file,
     &     nadactive_var, adactive_var, active_varnum,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     REVERSE_SIMULATION, myiter,bi,bj, mythid)

#endif

      end
