!=======================================================================
      subroutine comprecv_i4vec( dataname, length, vecFld )
      implicit none
! Predefined constants/arrays
#include "CPLR_SIG.h"
! MPI variables
#include "mpif.h"
! Arguments
      character*(*) dataname
      integer length
      integer vecFld(length)
! Functions
      integer generate_tag
      external generate_tag
! Local
      integer count,dtype,rank,tag,comm,ierr
      integer stat(MPI_STATUS_SIZE)
      integer i, j
      integer ibuf(MAX_IBUF)
!     ------------------------------------------------------------------

      if ( 1+length .gt. MAX_IBUF )
     &    STOP 'comprecv_i4vec: length exceeds MAX_IBUF'

! Receive message
      count = MAX_IBUF
      dtype = MPI_INTEGER
      rank = my_coupler_rank
      tag = generate_tag( 125, my_rank_in_global, dataname )
      comm = MPI_COMM_myglobal

      if (VERB) then
        write(LogUnit,*)
     &   'comprecv_i4vec: calling MPI_Recv rank=',rank
        write(LogUnit,*) 'comprecv_i4vec: dataname=',dataname
        call flush(LogUnit)
      endif
      call MPI_Recv( ibuf, count, dtype, rank, tag, comm, stat, ierr )
      if (VERB) then
        write(LogUnit,*) 'comprecv_i4vec: returned ierr=',ierr
        call flush(LogUnit)
      endif

      if (ierr.ne.0) then
        write(LogUnit,*) 'comprecv_i4vec: rank(W,G,L)=',
     &            my_rank_in_world,my_rank_in_global,my_rank_in_local,
     &            ' ierr=',ierr
        STOP 'comprecv_i4vec: MPI_Recv failed'
      endif

! Check header
      j = ibuf(1)
      if ( j.ne.length ) then
        write(LogUnit,*) 'comprecv_i4vec: length,header=', length, j
        STOP 'comprecv_i4vec: Incompatible header'
      endif

! Extract data
      do i=1,length
        vecFld(i) = ibuf(i+1)
      enddo

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
