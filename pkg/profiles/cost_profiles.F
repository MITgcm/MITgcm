#include "COST_CPPOPTIONS.h"

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
#include "cal.h"
#include "ecco_cost.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"
#ifdef ALLOW_PROFILES_CONTRIBUTION
#include "profiles.h"
#include "netcdf.inc"
#endif

c     == routine arguments ==
      integer myiter
      _RL     mytime
      integer mythid

#ifdef ALLOW_PROFILES_CONTRIBUTION

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
      fid=fiddata(num_file)
      do prof_num=1,NOBSGLOB
      if (prof_num.LE.ProfNo(num_file)) then

         if (xC(sNx+1,1,bi,bj).LT.xC(sNx+1,1,bi,bj)) then
         tmp_lon=2*xC(sNx,1,bi,bj)-xC(sNx-1,1,bi,bj)
         else
         tmp_lon=xC(sNx+1,1,bi,bj)
         endif

         if ((xC(1,1,bi,bj).LE.prof_lon(num_file,prof_num)).AND.
     &  (tmp_lon.GT.prof_lon(num_file,prof_num)).AND.
     &  (yC(1,1,bi,bj).LE.prof_lat(num_file,prof_num)).AND.
     &  (yC(1,sNy+1,bi,bj).GT.prof_lat(num_file,prof_num))
     & ) then

      do num_var=1,4

       do K=1,NLEVELMAX
       prof_traj1D(k)=0.
       prof_mask1D_cur(k)=0.
       prof_data1D(k)=0.
       prof_weights1D(k)=0.
       enddo

      if (vec_quantities(num_file,num_var).EQV..TRUE.) then
          call active_read_profile(num_file,
     &profdepthno(num_file),prof_traj1D,num_var,
     &prof_num,.false.,optimcycle,mythid,
     &profiles_dummy(num_file,num_var))
      call profiles_readvector(num_file,num_var,
     & prof_ind_glob(num_file,prof_num),profdepthno(num_file),
     & prof_data1D,myThid)
      call profiles_readvector(num_file,-num_var,
     & prof_ind_glob(num_file,prof_num),profdepthno(num_file),
     & prof_weights1D,myThid)
      do K=1,profdepthno(num_file)
         objf_profiles(num_file,num_var,bi,bj)=
     & objf_profiles(num_file,num_var,bi,bj)
     &     +prof_weights1D(K)*(prof_traj1D(K)-prof_data1D(K))*
     &     (prof_traj1D(K)-prof_data1D(K))*prof_mask1D_cur(K)
      if (prof_weights1D(K).GT.0.) then
         num_profiles(num_file,num_var,bi,bj)=
     & num_profiles(num_file,num_var,bi,bj)+prof_mask1D_cur(K)
      endif
      enddo
      endif

      enddo !do num_var... 
      endif !if ((xC(1,1,bi,bj).LE.prof_lon ...
      endif !if (prof_num.LE.ProfNo(num_file)) then
      enddo !do prof_num=..
      if (ProfNo(num_file).GT.0) then
        do num_var=1,4
          print*,'bi,bj,prof_num,num_var ',bi,bj,
     & ProfNo(num_file),num_var
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

      END
