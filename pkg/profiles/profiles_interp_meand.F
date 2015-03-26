#include "PROFILES_OPTIONS.h"

C     o==========================================================o
C     | subroutine profiles_interp                               |
C     | o 3D interpolation of model counterparts                 |
C     |   for netcdf profiles data                               |
C     | started: Gael Forget 15-March-2006                       |
C     o==========================================================o

      SUBROUTINE profiles_interp_meand(
     O traj_cur_out,
     I lon_cur,
     I lat_cur,
     I type_cur,
     I file_cur,
     I mytime,
     I bi,
     I bj,
     I myThid
     & )

      implicit none

C ==================== Global Variables ===========================
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
c#include "DYNVARS.h"
#include "PARAMS.h"
#ifdef ALLOW_CAL
#include "cal.h"
#endif
#ifdef ALLOW_PROFILES
# include "profiles.h"
#else
      integer NLEVELMAX
      parameter (NLEVELMAX=1)
#endif
c#ifdef ALLOW_PTRACERS
c#include "PTRACERS_SIZE.h"
c#include "PTRACERS_FIELDS.h"
c#endif
C ==================== Routine Variables ==========================
      _RL  mytime
      integer mythid
      integer type_cur,file_cur 
      _RL  traj_cur_out(NLEVELMAX)
      _RL  lon_cur,lat_cur

#ifdef ALLOW_PROFILES

C ==================== Local Variables ==========================
      _RL tab_coeffs1(2,2),tab_coeffs2(2,2),tab_coeffs3(2,2)
      _RL ponderations(2,2),pondsSUM,distance1,distance2
      integer i,j,k,kk,kcur,bi,bj
      _RL traj_cur(nR),mask_cur(nR)
      integer prof_i,prof_j
      _RL lon_tmp1,lon_tmp2,lon_1,lon_2,lat_1,lat_2,tmp_coeff
c--   == end of interface ==

       prof_i=-10
       prof_j=-10
       lon_1=-10
       lon_2=-10
       lat_1=-10
       lat_2=-10

        DO j=1,sNy+1
         DO i=1,sNx+1

cgf value of j, south of the data point:
      if (type_cur.NE.4) then
        if ((yC(i,j,bi,bj).LE.lat_cur).AND.
     &  (yC(i,j+1,bi,bj).GT.lat_cur)) then
          prof_j=j
          lat_1=yC(i,j,bi,bj)
          lat_2=yC(i,j+1,bi,bj)
        else
          prof_j=prof_j
          lat_1=lat_1
          lat_2=lat_2
        endif
      else
        if ((yG(i,j,bi,bj).LE.lat_cur).AND.
     &  (yG(i,j+1,bi,bj).GT.lat_cur)) then
          prof_j=j
          lat_1=yG(i,j,bi,bj)
          lat_2=yG(i,j+1,bi,bj)
        else
          prof_j=prof_j
          lat_1=lat_1
          lat_2=lat_2
        endif
      endif

cgf value of i, west of the data point:
      if (type_cur.NE.3) then
         if (xC(i+1,j,bi,bj).LT.xC(1,j,bi,bj)) then
         lon_tmp2=xC(i+1,j,bi,bj)+360
         else
         lon_tmp2=xC(i+1,j,bi,bj)
         endif
         if (xC(i,j,bi,bj).LT.xC(1,j,bi,bj)) then
         lon_tmp1=xC(i,j,bi,bj)+360
         else
         lon_tmp1=xC(i,j,bi,bj)
         endif
      else
         if (xG(i+1,j,bi,bj).LT.xG(1,j,bi,bj)) then
         lon_tmp2=xG(i+1,j,bi,bj)+360
         else
         lon_tmp2=xG(i+1,j,bi,bj)
         endif
         if (xG(i,j,bi,bj).LT.xG(1,j,bi,bj)) then
         lon_tmp1=xG(i,j,bi,bj)+360
         else
         lon_tmp1=xG(i,j,bi,bj)
         endif
      endif 
      if ((lon_tmp1.LE.lon_cur).AND.
     &(lon_tmp2.GT.lon_cur)) then
         prof_i=i
         lon_1=lon_tmp1
         lon_2=lon_tmp2
      else
         prof_i=prof_i
         lon_1=lon_1
         lon_2=lon_2
      endif

        ENDDO
       ENDDO


      if ((prof_i.NE.-10).AND.(prof_j.NE.-10)) then
cgf) spatial interpolation 

        distance1=(lat_cur-lat_1)/(lat_2-lat_1)
        distance2=(lon_cur-lon_1)/(lon_2-lon_1) 
        tab_coeffs2(1,1)=(1-distance2)*(1-distance1)
        tab_coeffs2(1,2)=distance2*(1-distance1)
        tab_coeffs2(2,1)=(1-distance2)*distance1
        tab_coeffs2(2,2)=distance2*distance1

       do k=1,nr
C
C       if (type_cur.EQ.6) then
C              tab_coeffs1(1,1)=prof_etan_mean(prof_i,prof_j,bi,bj) !SO
C              tab_coeffs1(1,2)=prof_etan_mean(prof_i+1,prof_j,bi,bj) !SE
C              tab_coeffs1(2,1)=prof_etan_mean(prof_i,prof_j+1,bi,bj) !NO
C              tab_coeffs1(2,2)=prof_etan_mean(prof_i+1,prof_j+1,bi,bj) !NZ
C              tab_coeffs3(1,1)=maskC(prof_i,prof_j,1,bi,bj) !SO
C              tab_coeffs3(1,2)=maskC(prof_i+1,prof_j,1,bi,bj) !SE
C              tab_coeffs3(2,1)=maskC(prof_i,prof_j+1,1,bi,bj) !NO
C              tab_coeffs3(2,2)=maskC(prof_i+1,prof_j+1,1,bi,bj) !NZ
C       elseif (type_cur .eq. 1) then

Cow only works for TS
        if (type_cur .eq. 1) then
          tab_coeffs1(1,1)=prof_tdat_mean(prof_i,prof_j,k,bi,bj) 
          tab_coeffs1(1,2)=prof_tdat_mean(prof_i+1,prof_j,k,bi,bj)
          tab_coeffs1(2,1)=prof_tdat_mean(prof_i,prof_j+1,k,bi,bj)
          tab_coeffs1(2,2)=prof_tdat_mean(prof_i+1,prof_j+1,k,bi,bj)
          tab_coeffs3(1,1)=maskC(prof_i,prof_j,k,bi,bj) 
          tab_coeffs3(1,2)=maskC(prof_i+1,prof_j,k,bi,bj)
          tab_coeffs3(2,1)=maskC(prof_i,prof_j+1,k,bi,bj) 
          tab_coeffs3(2,2)=maskC(prof_i+1,prof_j+1,k,bi,bj)
        elseif (type_cur .eq. 2) then
          tab_coeffs1(1,1)=prof_sdat_mean(prof_i,prof_j,k,bi,bj)
          tab_coeffs1(1,2)=prof_sdat_mean(prof_i+1,prof_j,k,bi,bj)
          tab_coeffs1(2,1)=prof_sdat_mean(prof_i,prof_j+1,k,bi,bj)
          tab_coeffs1(2,2)=prof_sdat_mean(prof_i+1,prof_j+1,k,bi,bj)
          tab_coeffs3(1,1)=maskC(prof_i,prof_j,k,bi,bj)
          tab_coeffs3(1,2)=maskC(prof_i+1,prof_j,k,bi,bj)
          tab_coeffs3(2,1)=maskC(prof_i,prof_j+1,k,bi,bj)
          tab_coeffs3(2,2)=maskC(prof_i+1,prof_j+1,k,bi,bj)
        else
               tab_coeffs1(1,1)=0.
               tab_coeffs1(2,1)=0.
               tab_coeffs1(1,2)=0.
               tab_coeffs1(2,2)=0.
               tab_coeffs3(1,1)=0.
               tab_coeffs3(2,1)=0.
               tab_coeffs3(1,2)=0.
               tab_coeffs3(2,2)=0.
        endif
       if(tab_coeffs1(1,1).EQ.0. _d 0) tab_coeffs3(1,1) = 0. _d 0
       if(tab_coeffs1(2,1).EQ.0. _d 0) tab_coeffs3(2,1) = 0. _d 0
       if(tab_coeffs1(1,2).EQ.0. _d 0) tab_coeffs3(1,2) = 0. _d 0
       if(tab_coeffs1(2,2).EQ.0. _d 0) tab_coeffs3(2,2) = 0. _d 0

        ponderations(1,1)=tab_coeffs3(1,1)*tab_coeffs2(1,1)
        ponderations(1,2)=tab_coeffs3(1,2)*tab_coeffs2(1,2)
        ponderations(2,1)=tab_coeffs3(2,1)*tab_coeffs2(2,1)
        ponderations(2,2)=tab_coeffs3(2,2)*tab_coeffs2(2,2)
        pondsSUM=ponderations(1,1)+ponderations(2,1)+ponderations(1,2)+
     &  ponderations(2,2)

        if (pondsSUM.GT.0) then
         tab_coeffs1(1,1)=tab_coeffs1(1,1)*ponderations(1,1)/pondsSUM
         tab_coeffs1(1,2)=tab_coeffs1(1,2)*ponderations(1,2)/pondsSUM
         tab_coeffs1(2,1)=tab_coeffs1(2,1)*ponderations(2,1)/pondsSUM
         tab_coeffs1(2,2)=tab_coeffs1(2,2)*ponderations(2,2)/pondsSUM
         traj_cur(k)=tab_coeffs1(1,1)+tab_coeffs1(2,1)+
     &   tab_coeffs1(1,2)+tab_coeffs1(2,2)   
         mask_cur(k)=1
        else
         traj_cur(k)=0
         mask_cur(k)=0
        endif
        if(k.GE.2.AND.mask_cur(k-1).EQ.0)then
         traj_cur(k)=0
         mask_cur(k)=0
        endif
       enddo

      else  
       do k=1,nr 
         traj_cur(k)=0
         mask_cur(k)=0
       enddo
      endif

cgf vertical interpolation:
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
     & 'ERROR in PROFILES_INTERP: unexpected case 1'
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
     & 'ERROR in PROFILES_INTERP: unexpected case 2'
             STOP 'ABNORMAL END: S/R PROFILES_INTERP'
            endif
          elseif (prof_depth(file_cur,kk,bi,bj).LT.-rF(kcur+1)) then
c  subcase 2: only 1 wet point just above=>upper cell value
            traj_cur_out(kk)=traj_cur(kcur)
            prof_mask1D_cur(kk,bi,bj)=mask_cur(kcur)
          endif
        endif
      enddo


#endif

      end

