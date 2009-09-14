C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/compsend_r8.F,v 1.3 2009/09/14 16:22:02 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine compsend_r8( dataname, Ni,Oi,Nj,Oj, arr )
      implicit none
! Arguments
      character*(*) dataname
      integer Ni,Oi,Nj,Oj
      real*8 arr(1-Oi:Ni+Oi,1-Oj:Nj+Oj)
! Predefined constants/arrays
#include "CPLR_SIG.h"
! MPI variables
#include "mpif.h"
      integer count,datatype,dest,tag,comm,ierr
! Functions
      integer generate_tag
! Local
      integer i,j,ij,Io,Jo
!     ------------------------------------------------------------------

      if (HEADER_SIZE+Ni*Nj.gt.MAX_R8_BUFLEN)
     &    stop 'compsend_r8: Nx*Ny too big'

! Set up buffer
      Io=my_tile_i0(1)
      Jo=my_tile_j0(1)
      r8buf(1)=float(Io)
      r8buf(2)=float(Ni)
      r8buf(3)=float(Jo)
      r8buf(4)=float(Nj)
      call mitcplr_char2dbl( dataname, r8buf(9) )

! Copy interior of tile to buffer
      do j=1,Nj
       do i=1,Ni
        ij=HEADER_SIZE+i+Ni*(j-1)
        r8buf(ij)=arr(i,j)
       enddo
      enddo

! Send message
      count=HEADER_SIZE+Ni*Nj
      datatype=MPI_DOUBLE_PRECISION
      dest=my_coupler_rank
      tag=generate_tag(102,my_rank_in_global,dataname)
      comm=MPI_COMM_myglobal

      if (VERB) then
       write(LogUnit,*) 'compsend_r8: calling MPI_Send dest=',dest
       write(LogUnit,*) 'compsend_r8: dataname=',dataname
       call flush(LogUnit)
      endif
      call MPI_Send( r8buf, count, datatype, dest, tag, comm, ierr )
      if (VERB) then
       write(LogUnit,*) 'compsend_r8: returned ierr=',ierr
       call flush(LogUnit)
      endif

      if (ierr.ne.0) then
       write(LogUnit,*) 'compsend_r8: rank(W,G,L)=',
     &            my_rank_in_world,my_rank_in_global,my_rank_in_local,
     &            ' ierr=',ierr
       stop 'compsend_r8: MPI_Send failed'
      endif

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
