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
     I   irecord, xG, yG,
     I   lon_0, lon_inc,
     I   lat_0, lat_inc,
     I   nx_in, ny_in, method, mythid)

C
C     infile       = name of the input file (direct access binary)
C     filePrec     = file precicision (currently not used, assumes real*4)
C     arrout       = output arrays (different for each processor)
C     irecord      = record number in global file
C     xG,yG        = coordinates for output grid
C     lon_0, lat_0 = lon and lat of sw corner of global input grid
C     lon_inc      = scalar x-grid increment
C     lat_inc      = vector y-grid increments
C     nx_in, ny_in = input x-grid and y-grid size
C     method       = 1 for bilinear 2 for bicubic
C     mythid       = thread id
C

#include "SIZE.h"
#include "EEPARAMS.h"

C subroutine variables
      character*(*) infile
      integer       filePrec, irecord, nx_in, ny_in
      _RL           arrayout(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS           xG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS           yG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL           lon_0, lon_inc
      _RL           lat_0, lat_inc(ny_in-1)
      integer       method, mythid

C local variables
      integer  ierr
      real*8   ne_fac,nw_fac,se_fac,sw_fac
      integer  e_ind(snx),w_ind(snx)
      integer  n_ind(sny),s_ind(sny)
      real*8   px_ind(4), py_ind(4), ew_val(4)
      external lagran
      real*8   lagran
      real*4   arrayin(-1:nx_in+2 ,      -1:ny_in+2)
      real*8   x_in   (-1:nx_in+2), y_in(-1:ny_in+2)
      integer  i, j, k, l, js, bi, bj, sp, interp_unit

C check input arguments
      if ( .NOT. (filePrec .EQ. 32) )
     & stop 'stop in exf_interp.F: value of filePrec not allowed'

C read in input data
      call mdsfindunit( interp_unit, mythid)
      open(interp_unit,file=infile,status='old',access='direct',
     & recl=nx_in*ny_in*4)
      read(interp_unit,rec=irecord) ((arrayin(i,j),i=1,nx_in),j=1,ny_in)
#ifdef _BYTESWAPIO
      call MDS_BYTESWAPR4((nx_in+4)*(ny_in+4), arrayin )
#endif
      close(interp_unit)

C setup input grid
      do i=-1,nx_in+2
       x_in(i) = lon_0 + (i-1.)*lon_inc
      enddo
      y_in(0) = lat_0 - lat_inc(1)
      y_in(-1)= lat_0 - 2.*lat_inc(1)
      y_in(1) = lat_0
      do j=2,ny_in
      y_in(j) = y_in(j-1) + lat_inc(j-1)
      enddo
      y_in(ny_in+1) = y_in(ny_in) + lat_inc(ny_in-1)
      y_in(ny_in+2) = y_in(ny_in) + 2.*lat_inc(ny_in-1)

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

      do bj = mybylo(mythid), mybyhi(mythid)
       do bi = mybxlo(mythid), mybxhi(mythid)

C check validity of input/output coordinates
        if ( xG(1,1  ,bi,bj) .le. x_in(0)       .or.
     &       xG(snx,1,bi,bj) .ge. x_in(nx_in+1) .or.
     &       yG(1,1  ,bi,bj) .lt. y_in(1)       .or.
     &       yG(1,sny,bi,bj) .gt. y_in(ny_in) ) then
           print*,'ERROR in S/R EXF_INTERP:'
           print*,'   input grid must encompass output grid.'
           STOP   ' ABNORMAL END: S/R EXF_INTERP'
        endif

C compute interpolation indices 
        do i=1,snx
         if (xG(i,1,bi,bj)-x_in(1) .ge. 0.) then
            w_ind(i) = int((xG(i,1,bi,bj)-x_in(1))/lon_inc) + 1
         else
            w_ind(i) = int((xG(i,1,bi,bj)-x_in(1))/lon_inc)
         endif
         e_ind(i) = w_ind(i) + 1
        enddo
        js = ny_in/2
        do j=1,sny
         do while (yG(1,j,bi,bj) .lt. y_in(js))
            js = (js + 1)/2
         enddo
         do while (yG(1,j,bi,bj) .ge. y_in(js+1))
            js = js + 1
         enddo
         s_ind(j) = js
         n_ind(j) = js + 1
        enddo

        if (method .eq. 1) then

C bilinear interpolation
         sp = 2
         do j=1,sny
          do i=1,snx
           arrayout(i,j,bi,bj) = 0.
           do l=0,1
            px_ind(l+1) = x_in(w_ind(i)+l)
            py_ind(l+1) = y_in(s_ind(j)+l)
           enddo
           do k=1,2
            ew_val(k) = arrayin(w_ind(i),s_ind(j)+k-1)
     &             *lagran(1,xG(i,1,bi,bj),px_ind,sp)
     &             +arrayin(e_ind(i),s_ind(j)+k-1)
     &             *lagran(2,xG(i,1,bi,bj),px_ind,sp)
            arrayout(i,j,bi,bj)=arrayout(i,j,bi,bj)
     &             +ew_val(k)*lagran(k,yG(1,j,bi,bj),py_ind,sp)
           enddo
          enddo
         enddo
        elseif (method .eq. 2) then

C bicubic interpolation
         sp = 4
         do j=1,sny
          do i=1,snx
           arrayout(i,j,bi,bj) = 0.
           do l=-1,2
            px_ind(l+2) = x_in(w_ind(i)+l)
            py_ind(l+2) = y_in(s_ind(j)+l)
           enddo
           do k=1,4
            ew_val(k) =
     &             arrayin(w_ind(i)-1,s_ind(j)+k-2)
     &             *lagran(1,xG(i,1,bi,bj),px_ind,sp)
     &             +arrayin(w_ind(i)  ,s_ind(j)+k-2)
     &             *lagran(2,xG(i,1,bi,bj),px_ind,sp)
     &             +arrayin(e_ind(i)  ,s_ind(j)+k-2)
     &             *lagran(3,xG(i,1,bi,bj),px_ind,sp) 
     &             +arrayin(e_ind(i)+1,s_ind(j)+k-2)
     &             *lagran(4,xG(i,1,bi,bj),px_ind,sp)
            arrayout(i,j,bi,bj)=arrayout(i,j,bi,bj) 
     &             +ew_val(k)*lagran(k,yG(1,j,bi,bj),py_ind,sp)
           enddo
          enddo
         enddo
        else
         stop 'stop in exf_interp.F: interpolation method not supported'
        endif
       enddo
      enddo

      END
