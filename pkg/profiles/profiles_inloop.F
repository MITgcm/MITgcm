C $Header: /u/gcmpack/MITgcm/pkg/profiles/profiles_inloop.F,v 1.12 2014/10/20 03:12:18 gforget Exp $
C $Name:  $

#include "PROFILES_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C     o==========================================================o
C     | subroutine profiles_inloop                               |
C     | o computes and writes model counterparts                 |
C     |   for netcdf profiles data                               |
C     | started: Gael Forget 15-March-2006                       |
C     o==========================================================o

      SUBROUTINE profiles_inloop(mytime,myThid )

      implicit none


C ==================== Global Variables ===========================
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#ifdef ALLOW_CAL
#include "cal.h"
#endif
#ifdef ALLOW_CTRL
#include "optim.h"
#endif
#ifdef ALLOW_PROFILES
# include "profiles.h"
# include "netcdf.inc"
#endif
C ==================== Routine Variables ==========================

      _RL mytime
      integer myThid

#ifdef ALLOW_PROFILES

C ==================== Local Variables ==========================
      integer k,bi,bj,prof_num, num_file, num_var
      _RL prof_traj1D(NLEVELMAX),prof_mask1D(NLEVELMAX)
#ifdef ALLOW_PROFILES_GENERICGRID
      integer prof_i1D(NUM_INTERP_POINTS),prof_j1D(NUM_INTERP_POINTS)
      _RL prof_w1D(NUM_INTERP_POINTS)
#endif
#ifndef ALLOW_CTRL
      integer optimcycle
#endif
      
c     == end of interface ==

#ifndef ALLOW_CTRL
      optimcycle = 0
#endif

        _BEGIN_MASTER( mythid )

      DO bj=1,nSy
       DO bi=1,nSx

      do num_file=1,NFILESPROFMAX
      do prof_num=1,NOBSGLOB
      if (prof_num.LE.ProfNo(num_file,bi,bj)) then
      if ((prof_time(num_file,prof_num,bi,bj).GE.mytime).AND.
     & (prof_time(num_file,prof_num,bi,bj).LT.(mytime+deltaTclock))) 
     & then

#ifdef ALLOW_PROFILES_GENERICGRID
      do k=1,NUM_INTERP_POINTS
      prof_i1D(k)= prof_interp_i(num_file,prof_num,k,bi,bj)
      prof_j1D(k)= prof_interp_j(num_file,prof_num,k,bi,bj)
      prof_w1D(k)= prof_interp_weights(num_file,prof_num,k,bi,bj)
      enddo
#endif

      do num_var=1,NVARMAX
      do k=1,NLEVELMAX
      prof_traj1D(k)=0
      prof_mask1D(k)=0
      enddo
      if (vec_quantities(num_file,num_var,bi,bj).EQV..TRUE.) then
#ifdef ALLOW_PROFILES_GENERICGRID
        if (.NOT.profilesDoGenGrid) then
#endif
          call profiles_interp(prof_traj1D,
     &        prof_lon(num_file,prof_num,bi,bj), 
     &        prof_lat(num_file,prof_num,bi,bj),
     &        num_var,num_file,mytime,bi,bj,myThid)
#ifdef ALLOW_PROFILES_GENERICGRID
        else
          call profiles_interp_gg(prof_traj1D,
     &        prof_i1D,prof_j1D,prof_w1D,
     &        num_var,num_file,mytime,bi,bj,myThid)
        endif
#endif
        call active_write_profile(num_file,
     &      ProfDepthNo(num_file,bi,bj),prof_traj1D,num_var,
     &      prof_num,optimcycle,bi,bj,mythid,
     &      profiles_dummy(num_file,num_var,bi,bj))
      endif
      enddo

      endif !if ((prof_time...
      endif !if (ProfNo(num_file,bi,bj).NE.0) then
      enddo !do prof_num...
      enddo !do num_file=1,NFILESPROFMAX
       ENDDO
      ENDDO

        _END_MASTER( mythid )

#endif

       END

