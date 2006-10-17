C $Header: /u/gcmpack/MITgcm/verification/global_with_exf/code/Attic/exf_interp_read.F,v 1.1 2006/10/17 18:24:47 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"
C note: Seems to run into problems in multi-threaded set-up
C       (at least with ifort) when using global pointer.
C       This version uses common block (no dynamic allocation)
C       and works fine in multi-threaded environement.
C       Will become part of the standard version in pkg/exf 
C       (with proper ifdef option)
#undef EXF_IREAD_USE_GLOBAL_POINTER

       SUBROUTINE EXF_INTERP_READ(
     I   infile, filePrec,
     O   arrayin,
     I   irecord, nx_in, ny_in, myThid)

      IMPLICIT NONE

C     infile       = name of the input file (direct access binary)
C     filePrec     = file precicision (currently not used, assumes real*4)
C     arrout       = output arrays (different for each processor)
C     irecord      = record number in global file
C     nx_in, ny_in = input x-grid and y-grid size
C     myThid       = thread id

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

C subroutine arguments
      CHARACTER*(*) infile
      INTEGER       filePrec, irecord, nx_in, ny_in
      real*4        arrayin(-1:nx_in+2 ,      -1:ny_in+2)
      INTEGER       myThid

C Functions
      INTEGER MDS_RECLEN

C local variables
      INTEGER  ierr, length_of_rec
      INTEGER  i, j, interp_unit
#ifdef EXF_IREAD_USE_GLOBAL_POINTER
      real*4, target ::   global(nx_in,ny_in)
#else
c     real*4   global(nx_in,ny_in)
      INTEGER    exf_interp_bufferSize
      PARAMETER( exf_interp_bufferSize = 10000 )
      real*4   global(exf_interp_bufferSize)
      COMMON /EXF_INTERP_BUFFER/ global
#endif

      _BARRIER
      _BEGIN_MASTER( myThid )

#ifndef EXF_IREAD_USE_GLOBAL_POINTER
C     The CPP symbol EXF_IREAD_USE_GLOBAL_POINTER must be defined for the
C     case of nThreads > 1. Stop if it isnt.
c     IF ( nThreads .GT. 1 ) THEN
c     STOP
c    &'EXF_INTERP_READ: nThreads > 1 needs EXF_IREAD_USE_GLOBAL_POINTER'
c     ENDIF
      IF ( nx_in*ny_in .GT. exf_interp_bufferSize ) THEN
        STOP 'EXF_INTERP_READ: exf_interp_bufferSize too small'
      ENDIF
#endif
C check input arguments
      IF ( filePrec .NE. 32 )
     &  STOP 'EXF_INTERP_READ: value of filePrec not allowed'

#ifdef ALLOW_USE_MPI
      IF ( .NOT.useSingleCPUIO .OR. mpiMyId.EQ.0 ) THEN
C master thread of process 0, only, opens a global file
#endif /* ALLOW_USE_MPI */

C read in input data
        CALL MDSFINDUNIT( interp_unit, myThid)
        length_of_rec=MDS_RECLEN( filePrec, nx_in*ny_in, myThid )
        OPEN(interp_unit,file=infile,status='old',access='direct',
     &        recl=length_of_rec)
#ifdef EXF_IREAD_USE_GLOBAL_POINTER
        READ(interp_unit,rec=irecord)  global
#else
        READ(interp_unit,rec=irecord) (global(i),i=1,nx_in*ny_in)
#endif
        CLOSE(interp_unit)

#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR4( nx_in*ny_in, global )
#endif /* _BYTESWAPIO */

#ifdef ALLOW_USE_MPI
      ENDIF

      IF ( useSingleCPUIO ) THEN
C broadcast to all processes
        CALL MPI_BCAST(global,nx_in*ny_in,MPI_REAL,
     &       0,MPI_COMM_MODEL,ierr)
      ENDIF
#endif /* ALLOW_USE_MPI */

#ifdef EXF_IREAD_USE_GLOBAL_POINTER
       glPtr => global
#endif
      _END_MASTER( myThid )
      _BARRIER

#ifdef EXF_IREAD_USE_GLOBAL_POINTER
       DO j=1,ny_in
        DO i=1,nx_in
         arrayin(i,j)=glPtr(i,j)
        ENDDO
       ENDDO
#else
       DO j=1,ny_in
        DO i=1,nx_in
         arrayin(i,j)=global(i+(j-1)*nx_in)
        ENDDO
       ENDDO
#endif

      RETURN
      END
