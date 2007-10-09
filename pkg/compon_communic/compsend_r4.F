C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/compsend_r4.F,v 1.2 2007/10/08 23:58:20 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine compsend_r4( dataname, Ni,Oi,Nj,Oj, arr )
      implicit none
! Arguments
      character*(*) dataname
      integer Ni,Oi,Nj,Oj
      real*4 arr(1-Oi:Ni+Oi,1-Oj:Nj+Oj)
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

      if (HEADER_SIZE+Ni*Nj.gt.MAX_R4_BUFLEN)
     &    stop 'compsend_r4: Nx*Ny too big'

! Set up buffer
      Io=my_tile_i0(1)
      Jo=my_tile_j0(1)
      r4buf(1)=float(Io)
      r4buf(2)=float(Ni)
      r4buf(3)=float(Jo)
      r4buf(4)=float(Nj)
      call mitcplr_char2real( dataname, r4buf(9) )

! Copy interior of tile to buffer
      do j=1,Nj
       do i=1,Ni
        ij=HEADER_SIZE+i+Ni*(j-1)
        r4buf(ij)=arr(i,j)
       enddo
      enddo

! Send message
      count=HEADER_SIZE+Ni*Nj
      datatype=MPI_REAL
      dest=my_coupler_rank
      tag=generate_tag(102,my_rank_in_global,dataname)
      comm=MPI_COMM_myglobal

      if (VERB) then
       write(LogUnit,*) 'compsend_r4: calling MPI_Send dest=',dest
       write(LogUnit,*) 'compsend_r4: dataname=',dataname
       call flush(LogUnit)
      endif
      call MPI_Send( r4buf, count, datatype, dest, tag, comm, ierr )
      if (VERB) then
       write(LogUnit,*) 'compsend_r4: returned ierr=',ierr
       call flush(LogUnit)
      endif

      if (ierr.ne.0) then
       write(LogUnit,*) 'compsend_r4: rank(W,G,L)=',
     &            my_rank_in_world,my_rank_in_global,my_rank_in_local,
     &            ' ierr=',ierr
       stop 'compsend_r4: MPI_Send failed'
      endif

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
