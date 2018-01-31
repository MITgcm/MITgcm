!=======================================================================
      subroutine coupsend_i4vec( component, dataname, length, vecFld )
      implicit none
! Predefined constants/arrays
#include "CPLR_SIG.h"
! MPI variables
#include "mpif.h"
! Arguments
      character*(*) component
      character*(*) dataname
      integer length
      integer vecFld(length)
! Functions
      integer mitcplr_match_comp
      integer generate_tag
      external mitcplr_match_comp
      external generate_tag
! Local
      integer count,dtype,dest,tag,comm,ierr
      integer compind, numprocs
      integer i, n
      integer ibuf(MAX_IBUF)
!     ------------------------------------------------------------------

      if ( 1+length .gt. MAX_IBUF )
     &    STOP 'coupsend_i4vec: length exceeds MAX_IBUF'

! Establish who I am communicating with
      compind = mitcplr_match_comp( component )
      if (compind.le.0) STOP 'coupsend_i4vec: Bad component id'
      comm = MPI_COMM_compcplr( compind )
      numprocs = num_component_procs(compind)
      if (numprocs.lt.1) then
        write(LogUnit,*) 'coupsend_i4vec: compind = ',compind
        STOP 'coupsend_i4vec: numprocs < 1'
      endif
      if (VERB)
     &  write(LogUnit,*) 'coupsend_i4vec: ',component_Name(compind)
      if (VERB)
     &  write(LogUnit,*) 'coupsend_i4vec: dataname=',dataname

! Copy vector to buffer
      ibuf(1) = length
      do i=1,length
        ibuf(i+1) = vecFld(i)
      enddo

! Foreach component process
      do n=1,numprocs

! Send message
       count = 1+length
       dtype = MPI_INTEGER
       tag = generate_tag( 125, n, dataname )
       dest = rank_component_procs(n,compind)

       if (VERB) then
        write(LogUnit,*)
     &    'coupsend_i4vec: calling MPI_Send dest=',dest,
     &    ' proc=',n,'/',numprocs
        call flush(LogUnit)
       endif
       call MPI_Send( ibuf, count, dtype, dest, tag, comm, ierr )
       if (VERB) then
        write(LogUnit,*) 'coupsend_i4vec: returned ierr=',ierr
        call flush(LogUnit)
       endif

       if (ierr.ne.0) then
        write(LogUnit,*) 'coupsend_i4vec: rank(W,G)=',
     &            my_rank_in_world,my_rank_in_global,
     &            ' ierr=',ierr
        STOP 'coupsend_i4vec: MPI_Send failed'
       endif

      enddo ! n

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
