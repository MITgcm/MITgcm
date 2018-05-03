! -*- fortran -*-
!
! Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
!                         University Research and Technology
!                         Corporation.  All rights reserved.
! Copyright (c) 2004-2005 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
!                         University of Stuttgart.  All rights reserved.
! Copyright (c) 2004-2005 The Regents of the University of California.
!                         All rights reserved.
! Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
! $COPYRIGHT$
! 
! Additional copyrights may follow
! 
! $HEADER$
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Do ***not*** copy this file to the directory where your Fortran 
! fortran application is compiled unless it is absolutely necessary!  Most
! modern Fortran compilers now support the -I command line flag, which
! tells the compiler where to find .h files (specifically, this one).  For
! example:
!
!      shell$ mpif77 foo.f -o foo -I$OMPI_HOME/include
!
! will probably do the trick (assuming that you have set OMPI_HOME 
! properly).
!
! That being said, OMPI's "mpif77" wrapper compiler should
! automatically include the -I option for you.  The following command
! should be equivalent to the command listed above:
!
!      shell$ mpif77 foo.f -o foo
!
! You should not copy this file to your local directory because it is
! possible that this file will be changed between versions of Open MPI.
! Indeed, this mpif.h is incompatible with the mpif.f of other 
! implementations of MPI.  Using this mpif.h with other implementations 
! of MPI, or with other versions of Open MPI will result in undefined
! behavior (to include incorrect results, segmentation faults, 
! unexplainable "hanging" in your application, etc.).  Always use the
! -I command line option instead (or let mpif77 do it for you).
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
!     Include the back-end file that has the bulk of the MPI Fortran
!     interface.
!

      include 'mpif-common.h'

!
!     These "external" statements are specific to the MPI F77 interface
!     (and are toxic to the MPI F90 interface), and are therefore in the
!     MPI F77-specific header file (i.e., this one).
!     
      external MPI_NULL_COPY_FN, MPI_NULL_DELETE_FN
      external MPI_COMM_NULL_COPY_FN, MPI_COMM_NULL_DELETE_FN
      external MPI_TYPE_NULL_COPY_FN, MPI_TYPE_NULL_DELETE_FN
      external MPI_DUP_FN, MPI_COMM_DUP_FN, MPI_TYPE_DUP_FN
      external MPI_WIN_NULL_COPY_FN
      external MPI_WIN_NULL_DELETE_FN
      external MPI_WIN_DUP_FN
!     Note that MPI_CONVERSION_FN_NULL is a "constant" (it is only ever
!     checked for comparison; it is never invoked), but it is passed as
!     a function pointer (to MPI_REGISTER_DATAREP) and therefore must be
!     the same size/type.  It is therefore external'ed here, and not
!     defined with an integer value in mpif-common.h.
      external MPI_CONVERSION_FN_NULL
      
!     
!     double precision functions
!     
      external MPI_WTIME, MPI_WTICK , PMPI_WTICK, PMPI_WTIME
      double precision MPI_WTIME, MPI_WTICK , PMPI_WTICK, PMPI_WTIME
      
