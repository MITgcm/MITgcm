C $Header: /u/gcmpack/MITgcm/pkg/profiles/cost_profiles.F,v 1.5 2006/07/14 20:19:36 gforget Exp $
C $Name:  $

#include "PROFILES_OPTIONS.h"

C     o==========================================================o
C     | subroutine cost_profiles				 |
C     | o computes the cost for netcdf profiles data		 |
C     | started: Gael Forget 15-March-2006                       |
C     o==========================================================o

      SUBROUTINE cost_profiles( myiter, mytime, myThid )

      IMPLICIT NONE

C     ======== Global data ============================
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_CAL
#include "cal.h"
#endif
#ifdef ALLOW_CTRL
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"
#endif
#ifdef ALLOW_PROFILES
#include "profiles.h"
#include "netcdf.inc"
#endif

c     == routine arguments ==
      integer myiter
      _RL     mytime
      integer mythid

#ifdef ALLOW_PROFILES
#ifdef ALLOW_COST

C     ========= Local variables =======================
      integer K,num_file,num_var,prof_num
      integer bi,bj,iG,jG,err,fid
      _RL tmp_lon
      _RL prof_traj1D(NLEVELMAX)
      _RL prof_data1D(NLEVELMAX), prof_weights1D(NLEVELMAX)
c     == end of interface ==
 
      DO bi = myBxLo(myThid), myBxHi(myThid)
      DO bj = myByLo(myThid), myByHi(myThid)

      do num_file=1,NFILESPROFMAX
      fid=fiddata(num_file,bi,bj)
      do prof_num=1,NOBSGLOB
      if (prof_num.LE.ProfNo(num_file,bi,bj)) then

      do num_var=1,4

       do K=1,NLEVELMAX
       prof_traj1D(k)=0.
       prof_mask1D_cur(k,bi,bj)=0.
       prof_data1D(k)=0.
       prof_weights1D(k)=0.
       enddo

      if (vec_quantities(num_file,num_var,bi,bj).EQV..TRUE.) then
          call active_read_profile(num_file,
     &ProfDepthNo(num_file,bi,bj),prof_traj1D,num_var,
     &prof_num,.false.,optimcycle,bi,bj,mythid,
     &profiles_dummy(num_file,num_var,bi,bj))
      call profiles_readvector(num_file,num_var,
     & prof_ind_glob(num_file,prof_num,bi,bj),
     & ProfDepthNo(num_file,bi,bj),prof_data1D,bi,bj,myThid)
      call profiles_readvector(num_file,-num_var,
     & prof_ind_glob(num_file,prof_num,bi,bj),
     & ProfDepthNo(num_file,bi,bj),prof_weights1D,bi,bj,myThid)
      do K=1,ProfDepthNo(num_file,bi,bj)
         objf_profiles(num_file,num_var,bi,bj)=
     & objf_profiles(num_file,num_var,bi,bj)
     &     +prof_weights1D(K)*(prof_traj1D(K)-prof_data1D(K))*
     &     (prof_traj1D(K)-prof_data1D(K))*prof_mask1D_cur(K,bi,bj)
      if (prof_weights1D(K).GT.0.) then
         num_profiles(num_file,num_var,bi,bj)=
     & num_profiles(num_file,num_var,bi,bj)+prof_mask1D_cur(K,bi,bj)
      endif
      enddo
      endif

      enddo !do num_var... 
      endif !if (prof_num.LE.ProfNo(num_file,bi,bj)) then
      enddo !do prof_num=..
      if (ProfNo(num_file,bi,bj).GT.0) then
        do num_var=1,4
          print*,'bi,bj,prof_num,num_var ',bi,bj,
     & ProfNo(num_file,bi,bj),num_var
          print*,prof_names(num_var),
     & objf_profiles(num_file,num_var,bi,bj),
     & num_profiles(num_file,num_var,bi,bj)
        enddo !do num_var...
      endif
      enddo !do num_file=1,NFILESPROFMAX

      ENDDO
      ENDDO
C===========================================================

#endif
#endif

      END
