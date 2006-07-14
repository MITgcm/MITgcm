C $Header: /u/gcmpack/MITgcm/pkg/profiles/profiles_inloop.F,v 1.6 2006/07/14 22:12:24 gforget Exp $
C $Name:  $

#include "PROFILES_OPTIONS.h"

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
      _RL tmp_lon,prof_traj1D(NLEVELMAX),prof_mask1D(NLEVELMAX)
#ifndef ALLOW_CTRL
      integer optimcycle
#endif
      
c     == end of interface ==

#ifndef ALLOW_CTRL
      optimcycle = 0
#endif

      DO bi = myBxLo(myThid), myBxHi(myThid)
      DO bj = myByLo(myThid), myByHi(myThid)

      do num_file=1,NFILESPROFMAX
      do prof_num=1,NOBSGLOB
      if (prof_num.LE.ProfNo(num_file,bi,bj)) then
      if ((prof_time(num_file,prof_num,bi,bj).GE.mytime).AND.
     & (prof_time(num_file,prof_num,bi,bj).LT.(mytime+deltaTclock))) 
     & then

      do num_var=1,6
      do k=1,NLEVELMAX
      prof_traj1D(k)=0
      prof_mask1D(k)=0
      enddo
      if (vec_quantities(num_file,num_var,bi,bj).EQV..TRUE.) then
	      call profiles_interp(prof_traj1D,
     & prof_lon(num_file,prof_num,bi,bj), 
     & prof_lat(num_file,prof_num,bi,bj),
     & num_var,num_file,mytime,bi,bj,myThid)
	       call active_write_profile(num_file,
     &ProfDepthNo(num_file,bi,bj),prof_traj1D,num_var,
     &prof_num,optimcycle,bi,bj,mythid,
     &profiles_dummy(num_file,num_var,bi,bj))
      endif
      enddo

      endif !if ((prof_time...
      endif !if (ProfNo(num_file,bi,bj).NE.0) then
      enddo !do prof_num...
      enddo !do num_file=1,NFILESPROFMAX
      ENDDO
      ENDDO

#endif

       END

