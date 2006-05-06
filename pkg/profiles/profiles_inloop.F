C $Header: /u/gcmpack/MITgcm/pkg/profiles/profiles_inloop.F,v 1.3 2006/05/06 14:33:53 heimbach Exp $
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
cph#include "ecco_cost.h"
cph#include "ctrl.h"
cph#include "ctrl_dummy.h"
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
      if (prof_num.LE.ProfNo(num_file)) then
      if ((prof_time(num_file,prof_num).GE.mytime).AND.
     & (prof_time(num_file,prof_num).LT.(mytime+deltaTclock))) then

      do num_var=1,4
      do k=1,NLEVELMAX
      prof_traj1D(k)=0
      prof_mask1D(k)=0
      enddo
      if (vec_quantities(num_file,num_var).EQV..TRUE.) then
	      call profiles_interp(prof_traj1D,
     & prof_lon(num_file,prof_num), prof_lat(num_file,prof_num),
     & num_var,num_file,mytime,myThid)
	       call active_write_profile(num_file,
     &profdepthno(num_file),prof_traj1D,num_var,
     &prof_num,optimcycle,mythid,profiles_dummy(num_file,num_var))
      endif
      enddo

      endif !if ((prof_time...
      endif !if (ProfNo(num_file).NE.0) then
      enddo !do prof_num...
      enddo !do num_file=1,NFILESPROFMAX
      ENDDO
      ENDDO

#endif

       END

