#include "EXF_OPTIONS.h"

       SUBROUTINE exf_interp_read(
     I   infile, filePrec,
     O   arrayin,
     I   irecord, nx_in, ny_in, mythid)

      implicit none

C     infile       = name of the input file (direct access binary)
C     filePrec     = file precicision (currently not used, assumes real*4)
C     arrout       = output arrays (different for each processor)
C     irecord      = record number in global file
C     nx_in, ny_in = input x-grid and y-grid size
C     mythid       = thread id

#include "SIZE.h"
#include "EEPARAMS.h"
#ifdef ALLOW_USE_MPI
# include "EESUPPORT.h"
#endif /* ALLOW_USE_MPI */
#include "PARAMS.h"

#ifdef EXF_IREAD_USE_GLOBAL_POINTER
C     When using threads the address of the local automatic array
C     "global" is not visible to the other threads. So we create
C     a pointer to share that address here. This is presently
C     in an ifdef because it won't go through g77 and I'm not
C     currently sure what TAF would do with this.
      COMMON /EXF_IOPTR/ glPtr
      REAL*4, POINTER :: glPtr(:,:)
#endif

C subroutine variables
      character*(*) infile
      integer       filePrec, irecord, nx_in, ny_in
      real*4        arrayin(-1:nx_in+2 ,      -1:ny_in+2)
      integer       mythid

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
#ifdef EXF_IREAD_USE_GLOBAL_POINTER
      real*4, target ::   global(nx_in,ny_in)
#else
      real*4   global(nx_in,ny_in)
#endif

      _BEGIN_MASTER( myThid )

#ifndef EXF_IREAD_USE_GLOBAL_POINTER
C     The CPP symbol EXF_IREAD_USE_GLOBAL_POINTER must be defined for the 
C     case of nThreads > 1. Stop if it isnt.
      IF ( nThreads .GT. 1 ) THEN
      STOP 
     &'EXF_INTERP_READ: nThreads > 1 needs EXF_IREAD_USE_GLOBAL_POINTER'
      ENDIF
#endif
C check input arguments
       if ( .NOT. (filePrec .EQ. 32) )
     &     stop 'stop in exf_interp.F: value of filePrec not allowed'

C read in input data
#ifdef ALLOW_USE_MPI
C      if (useSingleCPUIO) then
       if (.FALSE.) then

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
       else
#endif /* ALLOW_USE_MPI */

        call mdsfindunit( interp_unit, mythid)
        length_of_rec=MDS_RECLEN( filePrec, nx_in*ny_in, mythid )
        open(interp_unit,file=infile,status='old',access='direct',
     &       recl=length_of_rec)
        read(interp_unit,rec=irecord) global
        close(interp_unit)

#ifdef ALLOW_USE_MPI
       endif
#endif /* ALLOW_USE_MPI */
#ifdef EXF_IREAD_USE_GLOBAL_POINTER
       glPtr => global
#endif
      _END_MASTER( myThid )
      _BARRIER
#ifdef EXF_IREAD_USE_GLOBAL_POINTER
       do j=1,ny_in
        do i=1,nx_in
         arrayin(i,j)=glPtr(i,j)
        enddo
       enddo
#else
       do j=1,ny_in
        do i=1,nx_in
         arrayin(i,j)=global(i,j)
        enddo
       enddo
#endif

#ifdef _BYTESWAPIO
       call MDS_BYTESWAPR4((nx_in+4)*(ny_in+4), arrayin )
#endif /* _BYTESWAPIO */

      END
