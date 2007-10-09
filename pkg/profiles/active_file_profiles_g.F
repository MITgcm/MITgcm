C $Header: /u/gcmpack/MITgcm/pkg/profiles/active_file_profiles_g.F,v 1.6 2007/10/09 00:07:59 jmc Exp $
C $Name:  $

#include "PROFILES_OPTIONS.h"

c     ==================================================================
c
c     active_file_g.F: Routines to handle the I/O of the active file for
c                      the tangent linear calculations. All files are
c                      direct access files.
c
c     Routines
c
c    o  g_active_read_profile      - Read  an active 1D variable from file.
c    o  g_active_write_profile     - Write an active 1D variable to a file.
c
c        changed: gforget@ocean.mit.edu 23-Mar-2006
c
c     ==================================================================

      subroutine g_active_read_profile(
     I                           active_num_file,
     I                           nactive_var,
     O                           active_var,
     I                           active_varnum,
     I                           irec,
     I                           lAdInit,
     I                           myOptimIter,
     I                           bi,
     I                           bj,
     I                           mythid,
     I                           dummy,
     I                           g_active_var
     &                         )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE g_active_read_profile
c     ==================================================================
c     o Read an active 1D record from an profile data file.
c     ==================================================================

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_PROFILES
#include "profiles.h"
#endif

c     == routine arguments ==
c     active_var_file: filename
c     nactive_var:     integer size of active_var
c     active_var:      array
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
c     doglobalread:    flag for global or local read/write
c                      (default: .false.)
c     lAdInit:         initialisation of corresponding adjoint
c                      variable and write to active file
      integer nactive_var
      _RL  active_var(nactive_var)
      _RL  g_active_var(nactive_var)
      integer irec,active_varnum,active_num_file
      integer myOptimIter
      integer  bi,bj,mythid
      logical lAdInit
      _RL     dummy

#ifdef ALLOW_PROFILES

      call active_read_profile_rl( fidforward(active_num_file,bi,bj),
     &     active_num_file,
     &     nactive_var,  active_var, active_varnum, lAdInit,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     TANGENT_SIMULATION, myOptimIter,bi,bj, mythid)

      call active_read_profile_rl( fidtangent(active_num_file,bi,bj),
     &     active_num_file,
     &     nactive_var, g_active_var,active_varnum, lAdInit,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     TANGENT_SIMULATION, myOptimIter,bi,bj, mythid)

#endif

      return
      end


c     ==================================================================
c     ==================================================================
c     ==================================================================

      subroutine g_active_write_profile(
     I                           active_num_file,
     I                           nactive_var,
     I                           active_var,
     I                           active_varnum,
     I                           irec,
     I                           myOptimIter,
     I                           bi,
     I                           bj,
     I                           mythid,
     I                           dummy,
     I                           g_active_var,
     I                           g_dummy
     &                                )

c     ==================================================================
c     SUBROUTINE g_active_write_profile
c     ==================================================================
c
c     o Write an active 1D variable to a file.
c     ==================================================================

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_PROFILES
#include "profiles.h"
#endif

c     == routine arguments ==
c     active_var_file: filename
c     nactive_var:     integer size of active_var
c     active_var:      array
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
      integer nactive_var,active_num_file
      _RL  active_var(nactive_var)
      _RL  g_active_var(nactive_var)
      integer irec,active_varnum
      integer myOptimIter
      integer  bi,bj,mythid
      _RL     dummy
      _RL     g_dummy

#ifdef ALLOW_PROFILES

      call active_write_profile_rl( fidforward(active_num_file,bi,bj) ,
     &     active_num_file,
     &     nactive_var, active_var, active_varnum,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     TANGENT_SIMULATION, myOptimIter,bi,bj, mythid)

      call active_write_profile_rl( fidtangent(active_num_file,bi,bj) ,
     &     active_num_file,
     &     nactive_var, g_active_var, active_varnum,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     TANGENT_SIMULATION, myOptimIter,bi,bj, mythid)

#endif

      return
      end

c     ==================================================================



