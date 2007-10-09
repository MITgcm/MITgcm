C $Header: /u/gcmpack/MITgcm/pkg/profiles/active_file_profiles.F,v 1.6 2007/10/09 00:07:59 jmc Exp $
C $Name:  $

#include "PROFILES_OPTIONS.h"

c     ==================================================================
c
c     active_file.F: Routines to handle the I/O of the active file for
c                    the adjoint calculations. All files are direct
c                    access files.
c
c     Routines
c
c    o  active_read_profile       - Read  an active 1D record.
c    o  active_write_profile      - Write an active 1D record.
c
c
c        changed: gforget@ocean.mit.edu 23-Mar-2006
c
c     ==================================================================

C     !ROUTINE: active_read_profile
C     !INTERFACE:
      subroutine active_read_profile(
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
     I                           dummy
     &                         )

c     ==================================================================
c     SUBROUTINE active_read_profile
c     ==================================================================
c     o Read an active 1D record from an profile data file.
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
c     nactive_var:     integer size of active_var
c     active_var:      array
c     active_varname:      name fo the quantity to save
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
c     lAdInit:         initialisation of corresponding adjoint
c                      variable and write to active file
      integer  nactive_var,active_num_file
      _RL   active_var(nactive_var)
      integer  irec,active_varnum
      integer  myOptimIter
      integer  bi,bj,mythid
      logical  lAdInit
      _RL      dummy

#ifdef ALLOW_PROFILES

      call active_read_profile_rl( fidforward(active_num_file,bi,bj),
     &     active_num_file,
     &     nactive_var, active_var, active_varnum, lAdInit,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     FORWARD_SIMULATION, myOptimIter,bi,bj, mythid)

#endif

      return
      end

c     ==================================================================
c     ==================================================================
c     ==================================================================

C     !ROUTINE: active_write_profile
C     !INTERFACE:
      subroutine active_write_profile(
     I                           active_num_file,
     I                           nactive_var,
     I                           active_var,
     I                           active_varnum,
     I                           irec,
     I                           myOptimIter,
     I                           bi,
     I                           bj,
     I                           mythid,
     I                           dummy
     &                         )

C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE active_write_profile
c     ==================================================================
c     o Write an active 1D record from file.
c     ==================================================================

C     !USES:
      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_PROFILES
# include "profiles.h"
#endif

c     == routine arguments ==
c     active_var_file: filename
c     nactive_var:     integer size of active_var
c     active_var:      array
c     active_varname:      name fo the quantity to save
c     irec:            record number
c     myOptimIter:     number of optimization iteration (default: 0)
c     mythid:          thread number for this instance
      integer  nactive_var,active_num_file, active_varnum
      _RL   active_var(nactive_var)
      integer  irec
      integer  myOptimIter
      integer  bi,bj,mythid
      _RL      dummy

#ifdef ALLOW_PROFILES

      call active_write_profile_rl( fidforward(active_num_file,bi,bj),
     &     active_num_file,
     &     nactive_var, active_var, active_varnum,
     &     irec, prof_ind_glob(active_num_file,irec,bi,bj),
     &     FORWARD_SIMULATION, myOptimIter,bi,bj, mythid)

#endif

      return
      end

c     ==================================================================


