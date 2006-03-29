#include "COST_CPPOPTIONS.h"

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
#include "cal.h"
#include "ecco_cost.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"
#ifdef ALLOW_PROFILES_CONTRIBUTION
# include "profiles.h"
# include "netcdf.inc"
#endif
C ==================== Routine Variables ==========================

      _RL mytime
      integer myThid

#ifdef ALLOW_PROFILES_CONTRIBUTION

C ==================== Local Variables ==========================
      integer k,bi,bj,prof_num, num_file, num_var
      _RL tmp_lon,prof_traj1D(NLEVELMAX),prof_mask1D(NLEVELMAX)
      
c     == end of interface ==

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

