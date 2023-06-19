#include "PROFILES_OPTIONS.h"
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif

C     o==========================================================o
C     | subroutine profiles_interp                               |
C     | o 3D interpolation of model counterparts                 |
C     |   for netcdf profiles data                               |
C     | started: Gael Forget 15-March-2006                       |
C     o==========================================================o

      SUBROUTINE profiles_interp(
     O traj_cur_out,
     I i_cur,
     I j_cur,
     I weights_cur,
     I var_cur,
     I itr_cur,
     I file_cur,
     I myTime,
     I bi,
     I bj,
     I myThid
     & )

      implicit none

C ==================== Global Variables ===========================
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS_FIELDS.h"
#endif
#ifdef ALLOW_BLING
#include "BLING_VARS.h"
#endif
C ==================== Routine Variables ==========================
      _RL  myTime
      integer myThid
      integer file_cur, itr_cur
      character*(8) var_cur
#ifndef ALLOW_PROFILES
      _RL  traj_cur_out, weights_cur
      integer  i_cur, j_cur
#else
      _RL  traj_cur_out(NLEVELMAX)
      _RL  weights_cur(NUM_INTERP_POINTS)
      integer  i_cur(NUM_INTERP_POINTS)
      integer  j_cur(NUM_INTERP_POINTS)
#endif

C ==================== Local Variables ==========================
      _RL tab_coeffs1(NUM_INTERP_POINTS)
      _RL tab_coeffs3(NUM_INTERP_POINTS)
      _RL ponderations(NUM_INTERP_POINTS),pondsSUM
      integer q,k,kk,kcur,bi,bj
      _RL traj_cur(nR),mask_cur(nR)
      _RL tmp_coeff

c     == external functions ==
      integer ILNBLNK
      EXTERNAL ILNBLNK

c--   == end of interface ==

c horizontal interpolation:
      do k=1,nr
        pondsSUM=0. _d 0
        do q=1,NUM_INTERP_POINTS
        if (var_cur.EQ.'theta') then
               tab_coeffs1(q)=theta(i_cur(q),j_cur(q),k,bi,bj)
        elseif (var_cur.EQ.'salt') then
               tab_coeffs1(q)=salt(i_cur(q),j_cur(q),k,bi,bj)
        elseif (var_cur.EQ.'pTracer') then
#ifdef ALLOW_PTRACERS
               tab_coeffs1(q)=pTracer(i_cur(q),j_cur(q),k,bi,bj,
     &            itr_cur)
#else
               tab_coeffs1(q)=0. _d 0
#endif
#ifdef ALLOW_BLING
        elseif (var_cur.EQ.'PCO') then
               tab_coeffs1(q)=pCO2(i_cur(q),j_cur(q),bi,bj)
        elseif (var_cur.EQ.'PH') then
               tab_coeffs1(q)=pH(i_cur(q),j_cur(q),k,bi,bj)
        elseif (var_cur.EQ.'CHL') then
               tab_coeffs1(q)=CHL(i_cur(q),j_cur(q),k,bi,bj)
        elseif (var_cur.EQ.'POC') then
               tab_coeffs1(q)=POC(i_cur(q),j_cur(q),k,bi,bj)
#endif
#ifdef ALLOW_ECCO
        elseif (var_cur.EQ.'eta') then
               tab_coeffs1(q)=m_eta(i_cur(q),j_cur(q),bi,bj)
#endif
        elseif (var_cur.EQ.'UE') then
               tab_coeffs1(q)=m_UE(i_cur(q),j_cur(q),k,bi,bj)
        elseif (var_cur.EQ.'VN') then
               tab_coeffs1(q)=m_VN(i_cur(q),j_cur(q),k,bi,bj)
        else
               tab_coeffs1(q)=0. _d 0
        endif
        tab_coeffs3(q)=maskC(i_cur(q),j_cur(q),k,bi,bj)

        ponderations(q)=tab_coeffs3(q)*weights_cur(q)
        pondsSUM=pondsSUM+ponderations(q)
        enddo

        if (pondsSUM.GT.0) then
         traj_cur(k)=0. _d 0
         mask_cur(k)=1. _d 0
         do q=1,NUM_INTERP_POINTS
           traj_cur(k)=traj_cur(k)
     &     +tab_coeffs1(q)*ponderations(q)/pondsSUM
         enddo
        else
         traj_cur(k)=0. _d 0
         mask_cur(k)=0. _d 0
        endif
      enddo

c vertical interpolation:
      do kk=1,NLEVELMAX
         traj_cur_out(kk)=0
         prof_mask1D_cur(kk,bi,bj)=0
      enddo
      do kk=1,ProfDepthNo(file_cur,bi,bj)
c case 1: above first grid center=> first grid center value
        if (prof_depth(file_cur,kk,bi,bj).LT.-rC(1)) then
          traj_cur_out(kk)=traj_cur(1)
          prof_mask1D_cur(kk,bi,bj)=mask_cur(1)
c case 2: just below last grid center=> last cell value
        elseif (prof_depth(file_cur,kk,bi,bj).GE.-rC(nr)) then
          if ( prof_depth(file_cur,kk,bi,bj) .LT.
     &    (-rC(nr)+drC(nr)/2) ) then
            traj_cur_out(kk)=traj_cur(nr)
            prof_mask1D_cur(kk,bi,bj)=mask_cur(nr)
          endif
c case 3: between two grid centers
        else
          kcur=0
          do k=1,nr-1
            if ((prof_depth(file_cur,kk,bi,bj).GE.-rC(k)).AND.
     &      (prof_depth(file_cur,kk,bi,bj).LT.-rC(k+1))) then
              kcur=k
            endif
          enddo
          if (kcur.EQ.0) then
            WRITE(errorMessageUnit,'(A)')
     &        'ERROR in PROFILES_INTERP: unexpected case 1'
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R PROFILES_INTERP'
          endif
          if (mask_cur(kcur+1).EQ.1.) then
c  subcase 1: 2 wet points=>linear interpolation
            tmp_coeff=(prof_depth(file_cur,kk,bi,bj)+rC(kcur))/
     &      (-rC(kcur+1)+rC(kcur))
            traj_cur_out(kk)=(1-tmp_coeff)*traj_cur(kcur)
     &      +tmp_coeff*traj_cur(kcur+1)
            prof_mask1D_cur(kk,bi,bj)=1
            if (mask_cur(kcur).EQ.0.) then
            WRITE(errorMessageUnit,'(A)')
     &        'ERROR in PROFILES_INTERP: unexpected case 2'
            CALL ALL_PROC_DIE( myThid )
            STOP 'ABNORMAL END: S/R PROFILES_INTERP'
            endif
          elseif (prof_depth(file_cur,kk,bi,bj).LT.-rF(kcur+1)) then
c  subcase 2: only 1 wet point just above=>upper cell value
            traj_cur_out(kk)=traj_cur(kcur)
            prof_mask1D_cur(kk,bi,bj)=mask_cur(kcur)
          endif
        endif
      enddo

      RETURN
      END
