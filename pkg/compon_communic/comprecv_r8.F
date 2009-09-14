C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/comprecv_r8.F,v 1.3 2009/09/14 16:22:01 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine comprecv_r8( dataname, Ni,Oi,Nj,Oj, arr )
      implicit none
! Arguments
      character*(*) dataname
      integer Ni,Oi,Nj,Oj,Io,Jo
      real*8 arr(1-Oi:Ni+Oi,1-Oj:Nj+Oj)
! Predefined constants/arrays
#include "CPLR_SIG.h"
! MPI variables
#include "mpif.h"
      integer count,dtype,rank,tag,comm,ierr
      integer stat(MPI_STATUS_SIZE)
! Functions
      integer generate_tag
! Local
      integer i,j,ij,nx,ny
      character*(MAXLEN_COMP_NAME) recvdname
!     ------------------------------------------------------------------

      if (HEADER_SIZE+Ni*Nj.gt.MAX_R8_BUFLEN)
     &    stop 'comprecv_r8: Nx*Ny too big'

! Receive message
      count=HEADER_SIZE+MAX_R8_BUFLEN
      dtype=MPI_DOUBLE_PRECISION
      tag=generate_tag(121,my_rank_in_global,dataname)
      rank=my_coupler_rank
      comm=MPI_COMM_myglobal

      if (VERB) then
       write(LogUnit,*) 'comprecv_r8: calling MPI_Recv rank=',rank
       write(LogUnit,*) 'comprecv_r8: dataname=',dataname
       call flush(LogUnit)
      endif
      call MPI_Recv(r8buf, count, dtype, rank, tag, comm, stat, ierr)
      if (VERB) then
       write(LogUnit,*) 'comprecv_r8: returned ierr=',ierr
       call flush(LogUnit)
      endif

      if (ierr.ne.0) then
       write(LogUnit,*) 'comprecv_r8tiles: rank(W,G)=',
     &            my_rank_in_world,my_rank_in_global,
     &            ' ierr=',ierr
       stop 'comprecv_r8: MPI_Recv failed'
      endif

! Extract buffer
      Io=int(0.5+r8buf(1))
      Jo=int(0.5+r8buf(2))
      nx=int(0.5+r8buf(3))
      ny=int(0.5+r8buf(4))
      call mitcplr_dbl2char( r8buf(9), recvdname )

      if (Io.ne.my_tile_i0(1)) stop 'comprecv_r8: bad Io'
      if (Jo.ne.my_tile_j0(1)) stop 'comprecv_r8: bad Jo'
      if (nx.ne.my_tile_nx(1)) stop 'comprecv_r8: bad nx'
      if (ny.ne.my_tile_ny(1)) stop 'comprecv_r8: bad ny'
      if (recvdname .ne. dataname) then
       write(LogUnit,*) 'comprecv_r8: recvdname = ',recvdname
       write(LogUnit,*) 'comprecv_r8:  dataname = ',dataname
       stop 'comprecv_r8: recvdname != dataname'
      endif

! Copy buffer to interior of tile
      do j=1,Nj
       do i=1,Ni
        ij=HEADER_SIZE+i+Ni*(j-1)
        arr(i,j)=r8buf(ij)
       enddo
      enddo

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
