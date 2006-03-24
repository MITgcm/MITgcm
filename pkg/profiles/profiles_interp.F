#include "COST_CPPOPTIONS.h"

C     o==========================================================o
C     | subroutine profiles_interp                               |
C     | o 3D interpolation of model counterparts                 |
C     |   for netcdf profiles data                               |
C     | started: Gael Forget 15-March-2006                       |
C     o==========================================================o

      SUBROUTINE profiles_interp(
     O traj_cur_out,
     I lon_cur,
     I lat_cur,
     I type_cur,
     I file_cur,
     I mytime,
     I myThid
     & )

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
#else
      integer NLEVELMAX
      parameter (NLEVELMAX=1)
#endif
C ==================== Routine Variables ==========================
      _RL  mytime
      integer mythid
      integer type_cur,file_cur 
      _RL  traj_cur_out(NLEVELMAX)
      _RL  lon_cur,lat_cur

#ifdef ALLOW_PROFILES_CONTRIBUTION

C ==================== Local Variables ==========================
      _RL tab_coeffs1(2,2),tab_coeffs2(2,2),tab_coeffs3(2,2)
      _RL ponderations(2,2),pondsSUM,distance1,distance2
      integer i,j,k,kk,kcur,iG,jG,bi,bj
      _RL traj_cur(nR),mask_cur(nR)
      integer prof_i,prof_j
      _RL lon_tmp,tmp_coeff
c--   == end of interface ==

      DO bi = myBxLo(myThid), myBxHi(myThid)
      DO bj = myByLo(myThid), myByHi(myThid)
      prof_i=-10
      prof_j=-10

        DO j=1,sNy+1
         DO i=1,sNx+1
cgf value of j, south of the data point:
      if (type_cur.NE.4) then
               if ((yC(I,J,bi,bj).LE.lat_cur).AND.
     &(yC(I,J+1,bi,bj).GT.lat_cur)) then
                  prof_j=j
               endif
      else
               if ((yG(I,J,bi,bj).LE.lat_cur).AND.
     &(yG(I,J+1,bi,bj).GT.lat_cur)) then
                  prof_j=j
               endif
      endif
cgf value of i, west of the data point:
      if (type_cur.NE.3) then
         if (xC(i+1,j,bi,bj).LT.xC(i,j,bi,bj)) then
         lon_tmp=2*xC(i,j,bi,bj)-xC(i-1,j,bi,bj)
         else
         lon_tmp=xC(i+1,j,bi,bj)
         endif
               if ((xC(I,J,bi,bj).LE.lon_cur).AND.
     &(lon_tmp.GT.lon_cur)) then
                  prof_i=i
               endif
       else
         if (xG(i+1,j,bi,bj).LT.xG(i,j,bi,bj)) then
         lon_tmp=2*xG(i,j,bi,bj)-xG(i-1,j,bi,bj)
         else
         lon_tmp=xG(i+1,j,bi,bj)
         endif
               if ((xG(I,J,bi,bj).LE.lon_cur).AND.
     &(lon_tmp.GT.lon_cur)) then
                  prof_i=i
               endif
       endif
         ENDDO
        ENDDO


      if ((prof_i.NE.-10).AND.(prof_j.NE.-10)) then
cgf) spatial interpolation coefficients
c meridional direction:
      if (type_cur.NE.4) then
      distance1=(lat_cur-yC(prof_i,prof_j,bi,bj))
     &/(yC(prof_i,prof_j+1,bi,bj)-yC(prof_i,prof_j,bi,bj))
      else
      distance1=(lat_cur-yG(prof_i,prof_j,bi,bj))
     &/(yG(prof_i,prof_j+1,bi,bj)-yG(prof_i,prof_j,bi,bj))
      endif
c zonal direction:
      if (type_cur.NE.3) then
         if (xC(i+1,j,bi,bj).LT.xC(i,j,bi,bj)) then
         lon_tmp=2*xC(i,j,bi,bj)-xC(i-1,j,bi,bj)
         else
         lon_tmp=xC(i+1,j,bi,bj)
         endif
      distance2=(lon_cur-xC(prof_i,prof_j,bi,bj))
     &/(lon_tmp-xC(prof_i,prof_j,bi,bj))
      else
         if (xG(i+1,j,bi,bj).LT.xG(i,j,bi,bj)) then
         lon_tmp=2*xG(i,j,bi,bj)-xG(i-1,j,bi,bj)
         else
         lon_tmp=xG(i+1,j,bi,bj)
         endif
      distance2=(lon_cur-xG(prof_i,prof_j,bi,bj))
     &/(lon_tmp-xG(prof_i,prof_j,bi,bj))
      endif

      tab_coeffs2(1,1)=(1-distance2)*(1-distance1)
      tab_coeffs2(1,2)=distance2*(1-distance1)
      tab_coeffs2(2,1)=(1-distance2)*distance1
      tab_coeffs2(2,2)=distance2*distance1

cgf) mask at level k:
       do k=1,nr
       if (type_cur.EQ.1) then
               tab_coeffs1(1,1)=theta(prof_i,prof_j,k,bi,bj) !SO
               tab_coeffs1(1,2)=theta(prof_i+1,prof_j,k,bi,bj) !SE
               tab_coeffs1(2,1)=theta(prof_i,prof_j+1,k,bi,bj) !NO
               tab_coeffs1(2,2)=theta(prof_i+1,prof_j+1,k,bi,bj) !NZ              
               tab_coeffs3(1,1)=maskC(prof_i,prof_j,k,bi,bj) !SO
               tab_coeffs3(1,2)=maskC(prof_i+1,prof_j,k,bi,bj) !SE
               tab_coeffs3(2,1)=maskC(prof_i,prof_j+1,k,bi,bj) !NO
               tab_coeffs3(2,2)=maskC(prof_i+1,prof_j+1,k,bi,bj) !NZ
       elseif (type_cur.EQ.2) then
               tab_coeffs1(1,1)=salt(prof_i,prof_j,k,bi,bj) !SO
               tab_coeffs1(1,2)=salt(prof_i+1,prof_j,k,bi,bj) !SE
               tab_coeffs1(2,1)=salt(prof_i,prof_j+1,k,bi,bj) !NO
               tab_coeffs1(2,2)=salt(prof_i+1,prof_j+1,k,bi,bj) !NZ         
               tab_coeffs3(1,1)=maskC(prof_i,prof_j,k,bi,bj) !SO
               tab_coeffs3(1,2)=maskC(prof_i+1,prof_j,k,bi,bj) !SE
               tab_coeffs3(2,1)=maskC(prof_i,prof_j+1,k,bi,bj) !NO
               tab_coeffs3(2,2)=maskC(prof_i+1,prof_j+1,k,bi,bj) !NZ
       elseif (type_cur.EQ.3) then
               tab_coeffs1(1,1)=uVel(prof_i,prof_j,k,bi,bj) !SO
               tab_coeffs1(1,2)=uVel(prof_i+1,prof_j,k,bi,bj) !SE
               tab_coeffs1(2,1)=uVel(prof_i,prof_j+1,k,bi,bj) !NO
               tab_coeffs1(2,2)=uVel(prof_i+1,prof_j+1,k,bi,bj) !NZ
               tab_coeffs3(1,1)=maskW(prof_i,prof_j,k,bi,bj) !SO 
               tab_coeffs3(1,2)=maskW(prof_i+1,prof_j,k,bi,bj) !SE
               tab_coeffs3(2,1)=maskW(prof_i,prof_j+1,k,bi,bj) !NO
               tab_coeffs3(2,2)=maskW(prof_i+1,prof_j+1,k,bi,bj) !NZ 
       elseif (type_cur.EQ.4) then
               tab_coeffs1(1,1)=vVel(prof_i,prof_j,k,bi,bj) !SO
               tab_coeffs1(1,2)=vVel(prof_i+1,prof_j,k,bi,bj) !SE
               tab_coeffs1(2,1)=vVel(prof_i,prof_j+1,k,bi,bj) !NO
               tab_coeffs1(2,2)=vVel(prof_i+1,prof_j+1,k,bi,bj) !NZ
               tab_coeffs3(1,1)=maskS(prof_i,prof_j,k,bi,bj) !SO 
               tab_coeffs3(1,2)=maskS(prof_i+1,prof_j,k,bi,bj) !SE
               tab_coeffs3(2,1)=maskS(prof_i,prof_j+1,k,bi,bj) !NO
               tab_coeffs3(2,2)=maskS(prof_i+1,prof_j+1,k,bi,bj) !NZ 
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

         ponderations(1,1)=tab_coeffs3(1,1)*tab_coeffs2(1,1)
         ponderations(1,2)=tab_coeffs3(1,2)*tab_coeffs2(1,2)
         ponderations(2,1)=tab_coeffs3(2,1)*tab_coeffs2(2,1)
         ponderations(2,2)=tab_coeffs3(2,2)*tab_coeffs2(2,2)
         pondsSUM=ponderations(1,1)+ponderations(2,1)+ponderations(1,2)+
     &   ponderations(2,2)
      if (pondsSUM.GT.0) then
         tab_coeffs1(1,1)=tab_coeffs1(1,1)*ponderations(1,1)/pondsSUM
         tab_coeffs1(1,2)=tab_coeffs1(1,2)*ponderations(1,2)/pondsSUM
         tab_coeffs1(2,1)=tab_coeffs1(2,1)*ponderations(2,1)/pondsSUM
         tab_coeffs1(2,2)=tab_coeffs1(2,2)*ponderations(2,2)/pondsSUM
      traj_cur(k)=tab_coeffs1(1,1)+tab_coeffs1(2,1)+
     &        tab_coeffs1(1,2)+tab_coeffs1(2,2)   
      mask_cur(k)=1
      else
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
         prof_mask1D_cur(kk)=0
      enddo
      do kk=1,profdepthno(file_cur)
c case 1: above first grid center=> first grid center value 
      if (prof_depth(file_cur,kk).LT.-rC(1)) then
      traj_cur_out(kk)=traj_cur(1)
      prof_mask1D_cur(kk)=mask_cur(1)
c case 2: below last grid center
c		if in cell, last cell value
      elseif (prof_depth(file_cur,kk).GE.-rC(nr)) then
      if ( prof_depth(file_cur,kk) .LT. 
     & (-rC(nr)+drC(nr)/2) ) then  
      traj_cur_out(kk)=traj_cur(nr)
      prof_mask1D_cur(kk)=mask_cur(nr)
      endif
c case 3: between two grid centers
      else
      kcur=0
      do k=1,nr-1
      if ((prof_depth(file_cur,kk).GE.-rC(k)).AND.
     & (prof_depth(file_cur,kk).LT.-rC(k+1))) then
      kcur=k
      endif
      enddo
         if (kcur.EQ.0) then
      print*,"profiles_interp unexpected case: stop 1"
      stop
         endif
      if (mask_cur(kcur+1).EQ.1.) then
c	subcase1: 2 wet points
c		linear interpolation
      tmp_coeff=(prof_depth(file_cur,kk)+rC(kcur))/
     & (-rC(kcur+1)+rC(kcur))
      traj_cur_out(kk)=(1-tmp_coeff)*traj_cur(kcur)
     & +tmp_coeff*traj_cur(kcur+1)
      prof_mask1D_cur(kk)=1
         if (mask_cur(kcur).EQ.0.) then
      print*,"profiles_interp unexpected case: stop 2"
      stop
         endif
      elseif (prof_depth(file_cur,kk).LT.-rF(kcur+1)) then
c	subcase2: only upper is wet point
c		if in upper cell, upper cell value
      traj_cur_out(kk)=traj_cur(kcur)
      prof_mask1D_cur(kk)=mask_cur(kcur)
      endif
      endif
      enddo

      ENDDO
      ENDDO

#endif

      end

