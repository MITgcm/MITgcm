#include "EXF_OPTIONS.h"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Flux Coupler using                       C
C Bilinear interpolation of forcing fields C
C                                          C
C B. Cheng (12/2002)                       C
C                                          C
C added Bicubic (bnc 1/2003)               C
C                                          C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        real*8 function lagran(i,x,a,sp)

        INTEGER i,k,sp
        _RS x
        real*8 a(4)
        real*8 numer,denom

        numer = 1.D0
        denom = 1.D0

        do k=1,sp
        if ( k .ne. i) then
          denom = denom*(a(i) - a(k))
          numer = numer*(x    - a(k))
        endif
        enddo

        lagran = numer/denom

        return
        end


       SUBROUTINE exf_interp(
     I   infile,
     I   filePrec,
     O   arrayout,
     I   irecord, xG_in, yG,
     I   lon_0, lon_inc,
     I   lat_0, lat_inc,
     I   nx_in, ny_in, method, mythid)

      implicit none

C     infile       = name of the input file (direct access binary)
C     filePrec     = file precicision (currently not used, assumes real*4)
C     arrout       = output arrays (different for each processor)
C     irecord      = record number in global file
C     xG,yG        = coordinates for output grid
C     lon_0, lat_0 = lon and lat of sw corner of global input grid
C     lon_inc      = scalar x-grid increment
C     lat_inc      = vector y-grid increments
C     nx_in, ny_in = input x-grid and y-grid size
C     method       = 1,11,21 for bilinear; 2,12,22 for bicubic
C                    1,2 for tracer; 11,12 for U; 21,22 for V
C     mythid       = thread id
C

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C subroutine variables
      character*(*) infile
      integer       filePrec, irecord, nx_in, ny_in
      _RL           arrayout(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS           xG_in      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS           yG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL           lon_0, lon_inc
      _RL           lat_0, lat_inc(ny_in-1)
      integer       method, mythid

C local variables
      integer  e_ind(snx,sny),w_ind(snx,sny)
      integer  n_ind(snx,sny),s_ind(snx,sny)
      real*8   px_ind(4), py_ind(4), ew_val(4)
      external lagran
      real*8   lagran
      real*4   arrayin(-1:nx_in+2 ,      -1:ny_in+2)
      real*8   x_in   (-1:nx_in+2), y_in(-1:ny_in+2)
      real*8   ninety
      PARAMETER ( ninety = 90. )
      integer  i, j, k, l, js, bi, bj, sp, interp_unit
      _RS      xG(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS      threeSixtyRS, NorthValue
      PARAMETER ( threeSixtyRS = 360. )

C     put xG in interval [ lon_0 , lon_0+360 [
      do bj=myByLo(myThid),myByHi(myThid)
       do bi=myBxLo(myThid),myBxHi(myThid)
        do j=1-OLy,sNy+OLy
         do i=1-OLx,sNx+OLx
          xG(i,j,bi,bj) = xG_in(i,j,bi,bj)-lon_0
     &                  + threeSixtyRS*2.
          xG(i,j,bi,bj) = lon_0+mod(xG(i,j,bi,bj),threeSixtyRS)
         enddo
        enddo
       enddo
      enddo

       call exf_interp_read(
     I   infile, filePrec,
     O   arrayin,
     I   irecord, nx_in, ny_in, mythid)
      _BARRIER

C     _BEGIN_MASTER( myThid )

C setup input grid
       do i=-1,nx_in+2
        x_in(i) = lon_0 + (i-1)*lon_inc
       enddo

       y_in(0) = lat_0 - lat_inc(1)
       y_in(-1)= lat_0 - 2.*lat_inc(1)
       y_in(1) = lat_0
       do j=2,ny_in
        y_in(j) = y_in(j-1) + lat_inc(j-1)
       enddo
c       y_in(ny_in+1) = y_in(ny_in) + lat_inc(ny_in-1)
c       y_in(ny_in+2) = y_in(ny_in) + 2.*lat_inc(ny_in-1)
       y_in(ny_in+1) = min( y_in(ny_in) + lat_inc(ny_in-1), ninety )
       y_in(ny_in+2) = min( y_in(ny_in) + 2.*lat_inc(ny_in-1), ninety )

C enlarge boundary
       do j=1,ny_in
        arrayin(0,j)       = arrayin(nx_in,j)
        arrayin(-1,j)      = arrayin(nx_in-1,j)
        arrayin(nx_in+1,j) = arrayin(1,j)
        arrayin(nx_in+2,j) = arrayin(2,j)
       enddo
       do i=-1,nx_in+2
        arrayin(i,0)       = arrayin(i,1)
        arrayin(i,-1)      = arrayin(i,1)
        arrayin(i,ny_in+1) = arrayin(i,ny_in)
        arrayin(i,ny_in+2) = arrayin(i,ny_in) 
       enddo

C     For tracer (method=1,2) set to northernmost zonal-mean value
C     at 90N to avoid sharp zonal gradients near the Pole.
C     For U (method=11,12) set to zero at 90N to minimize velocity
C     gradient at North Pole
C     For V (method=11,12) set to northernmost zonal value at 90N,
C     as is already done above in order to allow cross-PoleArctic flow
       if (y_in(ny_in+1).eq.ninety) then
        if (method.eq.1 .or. method.eq.2) then
         NorthValue = 0
         do i=1,nx_in
          NorthValue = NorthValue + arrayin(i,ny_in)
         enddo
         NorthValue = NorthValue / nx_in
         do i=-1,nx_in+2
          arrayin(i,ny_in+1) = NorthValue
         enddo
        elseif (method.eq.11 .or. method.eq.12) then
         do i=-1,nx_in+2
          arrayin(i,ny_in+1) = 0
         enddo
        endif
       endif
       if (y_in(ny_in+2).eq.ninety) then
        if (method.eq.1 .or. method.eq.2) then
         NorthValue = 0
         do i=1,nx_in
          NorthValue = NorthValue + arrayin(i,ny_in)
         enddo
         NorthValue = NorthValue / nx_in
         do i=-1,nx_in+2
          arrayin(i,ny_in+2) = NorthValue
         enddo
        elseif (method.eq.11 .or. method.eq.12) then
         do i=-1,nx_in+2
          arrayin(i,ny_in+2) = 0
         enddo
        endif
       endif

C     _END_MASTER( myThid )
       
      do bj = mybylo(mythid), mybyhi(mythid)
       do bi = mybxlo(mythid), mybxhi(mythid)

C check validity of input/output coordinates
#ifdef ALLOW_DEBUG
        if ( debugLevel .ge. debLevB ) then
         do i=1,snx
          do j=1,sny
           if ( xG(i,j,bi,bj) .lt. x_in(0)         .or.
     &          xG(i,j,bi,bj) .ge. x_in(nx_in+1)   .or.
     &          yG(i,j,bi,bj) .lt. y_in(0)         .or.
     &          yG(i,j,bi,bj) .ge. y_in(ny_in+1) ) then
              print*,'ERROR in S/R EXF_INTERP:'
              print*,'   input grid must encompass output grid.'
              print*,'i,j,bi,bj'      ,i,j,bi,bj
              print*,'xG,yG'          ,xG(i,j,bi,bj),yG(i,j,bi,bj)
              print*,'nx_in,ny_in'    ,nx_in        ,ny_in
              print*,'x_in(0,nx_in+1)',x_in(0)      ,x_in(nx_in+1)
              print*,'y_in(0,ny_in+1)',y_in(0)      ,y_in(ny_in+1)
              STOP   ' ABNORMAL END: S/R EXF_INTERP'
             endif
           enddo
         enddo
        endif
#endif /* ALLOW_DEBUG */

C compute interpolation indices 
        do i=1,snx
         do j=1,sny
          if (xG(i,j,bi,bj)-x_in(1) .ge. 0.) then
           w_ind(i,j) = int((xG(i,j,bi,bj)-x_in(1))/lon_inc) + 1
          else
           w_ind(i,j) = int((xG(i,j,bi,bj)-x_in(1))/lon_inc)
          endif
          e_ind(i,j) = w_ind(i,j) + 1
          js = ny_in*.5
          do while (yG(i,j,bi,bj) .lt. y_in(js))
           js = (js - 1)*.5
          enddo
          do while (yG(i,j,bi,bj) .ge. y_in(js+1))
           js = js + 1
          enddo
          s_ind(i,j) = js
          n_ind(i,j) = js + 1
         enddo
        enddo

        if (method.eq.1 .or. method.eq.11 .or. method.eq.21) then

C bilinear interpolation
         sp = 2
         do j=1,sny
          do i=1,snx
           arrayout(i,j,bi,bj) = 0.
           do l=0,1
            px_ind(l+1) = x_in(w_ind(i,j)+l)
            py_ind(l+1) = y_in(s_ind(i,j)+l)
           enddo
           do k=1,2
            ew_val(k) = arrayin(w_ind(i,j),s_ind(i,j)+k-1)
     &             *lagran(1,xG(i,j,bi,bj),px_ind,sp)
     &             +arrayin(e_ind(i,j),s_ind(i,j)+k-1)
     &             *lagran(2,xG(i,j,bi,bj),px_ind,sp)
            arrayout(i,j,bi,bj)=arrayout(i,j,bi,bj)
     &             +ew_val(k)*lagran(k,yG(i,j,bi,bj),py_ind,sp)
           enddo
          enddo
         enddo
        elseif (method .eq. 2 .or. method.eq.12 .or. method.eq.22) then

C bicubic interpolation
         sp = 4
         do j=1,sny
          do i=1,snx
           arrayout(i,j,bi,bj) = 0.
           do l=-1,2
            px_ind(l+2) = x_in(w_ind(i,j)+l)
            py_ind(l+2) = y_in(s_ind(i,j)+l)
           enddo
           do k=1,4
            ew_val(k) =
     &             arrayin(w_ind(i,j)-1,s_ind(i,j)+k-2)
     &             *lagran(1,xG(i,j,bi,bj),px_ind,sp)
     &             +arrayin(w_ind(i,j)  ,s_ind(i,j)+k-2)
     &             *lagran(2,xG(i,j,bi,bj),px_ind,sp)
     &             +arrayin(e_ind(i,j)  ,s_ind(i,j)+k-2)
     &             *lagran(3,xG(i,j,bi,bj),px_ind,sp) 
     &             +arrayin(e_ind(i,j)+1,s_ind(i,j)+k-2)
     &             *lagran(4,xG(i,j,bi,bj),px_ind,sp)
            arrayout(i,j,bi,bj)=arrayout(i,j,bi,bj) 
     &             +ew_val(k)*lagran(k,yG(i,j,bi,bj),py_ind,sp)
           enddo
          enddo
         enddo
        else
         stop 'stop in exf_interp.F: interpolation method not supported'
        endif
       enddo
      enddo

      END
