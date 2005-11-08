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

       SUBROUTINE exf_interp_read(
     I   infile,
     I   filePrec,
     O   arrayin,
     I   irecord, xG, yG,
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
C     method       = 1 for bilinear 2 for bicubic
C     mythid       = thread id
C

#include "SIZE.h"
#include "EEPARAMS.h"
#ifdef ALLOW_USE_MPI
# include "EESUPPORT.h"
#endif /* ALLOW_USE_MPI */
#include "PARAMS.h"

C subroutine variables
      character*(*) infile
      integer       filePrec, irecord, nx_in, ny_in
      real*4        arrayin(-1:nx_in+2 ,      -1:ny_in+2)
      _RS           xG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS           yG      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL           lon_0, lon_inc
      _RL           lat_0, lat_inc(ny_in-1)
      integer       method, mythid

C Functions
      integer MDS_RECLEN

C local variables
      integer  ierr, length_of_rec
      real*8   ne_fac,nw_fac,se_fac,sw_fac
      integer  e_ind(snx,sny),w_ind(snx,sny)
      integer  n_ind(snx,sny),s_ind(snx,sny)
      real*8   px_ind(4), py_ind(4), ew_val(4)
      external lagran
      real*8   lagran
      integer  i, j, k, l, js, bi, bj, sp, interp_unit
      real*4   global(nx_in,ny_in)

C     _BEGIN_MASTER( myThid )

C check input arguments
       if ( .NOT. (filePrec .EQ. 32) )
     &     stop 'stop in exf_interp.F: value of filePrec not allowed'

C read in input data
#ifdef ALLOW_USE_MPI
       if (useSingleCPUIO) then

C master thread of process 0, only, opens a global file
        IF( mpiMyId .EQ. 0 ) THEN
         call mdsfindunit( interp_unit, mythid)
         length_of_rec=MDS_RECLEN( filePrec, nx_in*ny_in, mythid )
         open(interp_unit,file=infile,status='old',access='direct',
     &        recl=length_of_rec)
         read(interp_unit,rec=irecord)
     &        ((global(i,j),i=1,nx_in),j=1,ny_in)
         close(interp_unit)
        ENDIF

C broadcast to all processes
        call MPI_BCAST(global,nx_in*ny_in,MPI_REAL,
     &       0,MPI_COMM_MODEL,ierr)
        do j=1,ny_in
         do i=1,nx_in
          arrayin(i,j)=global(i,j)
         enddo
        enddo

       else
#endif /* ALLOW_USE_MPI */

        call mdsfindunit( interp_unit, mythid)
        length_of_rec=MDS_RECLEN( filePrec, nx_in*ny_in, mythid )
        open(interp_unit,file=infile,status='old',access='direct',
     &       recl=length_of_rec)
        read(interp_unit,rec=irecord)
     &       ((arrayin(i,j),i=1,nx_in),j=1,ny_in)
        close(interp_unit)

#ifdef ALLOW_USE_MPI
       endif
#endif /* ALLOW_USE_MPI */

#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR4((nx_in+4)*(ny_in+4), arrayin )
#endif /* _BYTESWAPIO */

C     _END_MASTER( myThid )

      END
