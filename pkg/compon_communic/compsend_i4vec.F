C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/compsend_i4vec.F,v 1.1 2013/12/02 21:32:39 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine compsend_i4vec( dataname, length, vecFld )
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
      integer count,datatype,dest,tag,comm,ierr
      integer i
      integer ibuf(MAX_IBUF)
!     ------------------------------------------------------------------

      if ( 1+length .gt. MAX_IBUF )
     &    STOP 'compsend_i4vec: length exceeds MAX_IBUF'

! Copy vector to buffer
      ibuf(1) = length
      do i=1,length
        ibuf(i+1) = vecFld(i)
      enddo

! Send message
      count = 1+length
      datatype = MPI_INTEGER
      dest = my_coupler_rank
      tag = generate_tag( 115, my_rank_in_global, dataname )
      comm = MPI_COMM_myglobal

      if (VERB) then
        write(LogUnit,*)
     &   'compsend_i4vec: calling MPI_Send dest=',dest
        write(LogUnit,*) 'compsend_i4vec: dataname=',dataname
        call flush(LogUnit)
      endif
      call MPI_Send( ibuf, count, datatype, dest, tag, comm, ierr )
      if (VERB) then
        write(LogUnit,*) 'compsend_i4vec: returned ierr=',ierr
        call flush(LogUnit)
      endif

      if (ierr.ne.0) then
        write(LogUnit,*) 'compsend_i4vec: rank(W,G,L)=',
     &            my_rank_in_world,my_rank_in_global,my_rank_in_local,
     &            ' ierr=',ierr
        STOP 'compsend_i4vec: MPI_Send failed'
      endif

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
