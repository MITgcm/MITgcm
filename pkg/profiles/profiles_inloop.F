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

      SUBROUTINE profiles_inloop(myTime,myThid )

      IMPLICIT NONE

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
#include "OPTIMCYCLE.h"
#endif
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
# include "netcdf.inc"
#endif
C ==================== Routine Variables ==========================

      _RL myTime
      integer myThid

#ifdef ALLOW_PROFILES

C ==================== Local Variables ==========================
      integer k,bi,bj,prof_num, num_file, num_var
      _RL prof_traj1D(NLEVELMAX),prof_mask1D(NLEVELMAX)
      integer prof_i1D(NUM_INTERP_POINTS),prof_j1D(NUM_INTERP_POINTS)
      _RL prof_w1D(NUM_INTERP_POINTS)
#ifndef ALLOW_CTRL
      integer optimcycle
#endif
#ifndef ALLOW_ECCO
      integer i,j
#endif

c     == end of interface ==

#ifndef ALLOW_CTRL
      optimcycle = 0
#endif

        _BEGIN_MASTER( myThid )

#ifndef ALLOW_ECCO
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          do k = 1,Nr
            do j = 1,sNy
              do i =  1,sNx
                m_UE(i,j,k,bi,bj)=0. _d 0
                m_VN(i,j,k,bi,bj)=0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo

      CALL ROTATE_UV2EN_RL(
     U          uVel, vVel, m_UE, m_VN,
     I          .TRUE., .TRUE., .FALSE., Nr, myThid )
#endif /* ALLOW_ECCO */

      DO bj=1,nSy
       DO bi=1,nSx

      do num_file=1,NFILESPROFMAX
      do prof_num=1,NOBSGLOB
      if (prof_num.LE.ProfNo(num_file,bi,bj)) then
      if ((prof_time(num_file,prof_num,bi,bj).GE.myTime).AND.
     & (prof_time(num_file,prof_num,bi,bj).LT.(myTime+deltaTClock)))
     & then

      do k=1,NUM_INTERP_POINTS
      prof_i1D(k)= prof_interp_i(num_file,prof_num,k,bi,bj)
      prof_j1D(k)= prof_interp_j(num_file,prof_num,k,bi,bj)
      prof_w1D(k)= prof_interp_weights(num_file,prof_num,k,bi,bj)
      enddo

      do num_var=1,NVARMAX
      do k=1,NLEVELMAX
      prof_traj1D(k)=0
      prof_mask1D(k)=0
      enddo
      if (vec_quantities(num_file,num_var,bi,bj).EQV..TRUE.) then
        call profiles_interp(prof_traj1D,
     &        prof_i1D,prof_j1D,prof_w1D,
     &        prof_namesmod(num_file,num_var),
     &        prof_itracer(num_file,num_var),
     &        num_file,myTime,bi,bj,myThid)
        do k=1,NLEVELMAX
          prof_traj1D(k)=prof_traj1D(k)*
     &                   prof_facmod(num_file,num_var)
        enddo
        call active_write_profile(num_file,
     &      ProfDepthNo(num_file,bi,bj),prof_traj1D,num_var,
     &      prof_num,optimcycle,bi,bj,myThid,
     &      profiles_dummy(num_file,num_var,bi,bj))
      endif
      enddo

      endif !if ((prof_time...
      endif !if (ProfNo(num_file,bi,bj).NE.0) then
      enddo !do prof_num...
      enddo !do num_file=1,NFILESPROFMAX
       ENDDO
      ENDDO

        _END_MASTER( myThid )

#endif

       END

