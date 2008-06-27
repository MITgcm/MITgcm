C $Header: /u/gcmpack/MITgcm/pkg/profiles/active_file_profiles_ad.F,v 1.7 2008/06/27 19:04:14 heimbach Exp $
C $Name:  $

#include "PROFILES_OPTIONS.h"

c     ==================================================================
c
c     active_files_ad.F: Routines to handle the I/O of the active file
c                        for the adjoint calculations. All files are
c                        direct access files.
c
c     Routines
c
c     o  adactive_read_profile      - Adjoint of active_read_profile
c     o  adactive_write_profile     - Adjoint of active_write_profile
c
c        changed: gforget@ocean.mit.edu 23-Mar-2006
c
c     ==================================================================

      subroutine adactive_read_profile(
     I                              active_num_file,
     I                              nadactive_var,
     I                              adactive_var,
     I                              active_varnum,
     I                              irec,
     I                              lAdInit,
     I                              myiter,
     I                              bi,
     I                              bj,
     I                              mythid
     &                            )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE adactive_read_profile
c     ==================================================================
c     o Adjoint of active_read_profile.
c     ==================================================================
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_PROFILES
# include "profiles.h"
#endif

c     == routine arguments ==
c     active_var_file: filename
c     nadactive_var:   integer size of adactive_var
c     adactive_var:    array
c     irec:            record number
c     myIter:          number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
c     doglobalread:    flag for global or local read/write
c                      (default: .false.)
c     lAdInit:         initialisation of corresponding adjoint
c                      variable and write to active file
      integer nadactive_var,active_num_file
      _RL    adactive_var(nadactive_var)
      integer irec,active_varnum
      integer myiter
      integer  bi,bj,mythid
      logical lAdInit

#ifdef ALLOW_PROFILES

      call active_read_profile_rl( fidadjoint(active_num_file,bi,bj) ,
     &     active_num_file,
     &     nadactive_var, adactive_var, active_varnum, lAdInit,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     REVERSE_SIMULATION, myiter,bi,bj, mythid)

#endif

      end

c     ==================================================================
c     ==================================================================
c     ==================================================================


      subroutine adactive_write_profile(
     I                              active_num_file,
     I                              nadactive_var,
     I                              adactive_var,
     I                              active_varnum,
     I                              irec,
     I                              myiter,
     I                              bi,
     I                              bj,
     I                              mythid,
     I                              dummy
     &                            )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE adactive_write_profile
c     ==================================================================
c     o Adjoint of active_write_profile.
c     ==================================================================

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_PROFILES
# include "profiles.h"
#endif

c     == routine arguments ==
c     active_var_file: filename
c     nadactive_var:   integer size of adactive_var
c     adactive_var:    array
c     irec:            record number
c     myIter:          number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
c     doglobalread:    flag for global or local read/write
c                      (default: .false.)
c     lAdInit:         initialisation of corresponding adjoint
c                      variable and write to active file
      integer nadactive_var,active_num_file
      _RL    adactive_var(nadactive_var)
      integer irec,active_varnum
      integer myiter
      integer  bi,bj,mythid
      _RL     dummy


c     == end of interface ==

#ifdef ALLOW_PROFILES

      call active_write_profile_rl( fidadjoint(active_num_file,bi,bj) ,
     &     active_num_file,
     &     nadactive_var, adactive_var, active_varnum,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     REVERSE_SIMULATION, myiter,bi,bj, mythid)

#endif

      end

c     ==================================================================


