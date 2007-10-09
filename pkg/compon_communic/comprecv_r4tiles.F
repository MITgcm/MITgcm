C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/comprecv_r4tiles.F,v 1.2 2007/10/08 23:58:20 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine comprecv_r4tiles( dataname, Ni,Oi,Nj,Oj,Nk,Tx,Ty, arr )
      implicit none
! Arguments
      character*(*) dataname
      integer Ni,Oi,Nj,Oj,Io,Jo,Nk,Tx,Ty
      real*4 arr(1-Oi:Ni+Oi,1-Oj:Nj+Oj,Nk,Tx,Ty)
! Predefined constants/arrays
#include "CPLR_SIG.h"
! MPI variables
#include "mpif.h"
      integer count,dtype,rank,tag,comm,ierr
      integer stat(MPI_STATUS_SIZE)
! Functions
      integer generate_tag
! Local
      integer i,j,ij,nx,ny,k,bibj,bi,bj
      character*(MAXLEN_COMP_NAME) recvdname
!     ------------------------------------------------------------------

      if (HEADER_SIZE+Ni*Nj.gt.MAX_R4_BUFLEN)
     &    stop 'comprecv_r4tiles: Nx*Ny too big'

! Foreach tile which is non-blank
      do bibj=1,my_num_tiles

       bi=my_tile_bi(bibj)
       bj=my_tile_bj(bibj)

! Receive message
       count=HEADER_SIZE+MAX_R4_BUFLEN
       dtype=MPI_REAL
       tag=generate_tag(123,bibj,dataname)
       rank=my_coupler_rank
       comm=MPI_COMM_myglobal

       if (VERB) then
        write(LogUnit,*) 'comprecv_r4tiles: calling MPI_Recv rank=',rank
        write(LogUnit,*) 'comprecv_r4tiles: dataname=',dataname
        call flush(LogUnit)
       endif
       call MPI_Recv(r4buf, count, dtype, rank, tag, comm, stat, ierr)
       if (VERB) then
        write(LogUnit,*) 'comprecv_r4tiles: returned ierr=',ierr
        call flush(LogUnit)
       endif

       if (ierr.ne.0) then
        write(LogUnit,*) 'comprecv_r4tiles: rank(W,G)=',
     &            my_rank_in_world,my_rank_in_global,
     &            ' ierr=',ierr
        stop 'comprecv_r4tiles: MPI_Recv failed'
       endif

! Extract buffer
       Io=int(0.5+r4buf(1))
       Jo=int(0.5+r4buf(2))
       nx=int(0.5+r4buf(3))
       ny=int(0.5+r4buf(4))
       call mitcplr_real2char( r4buf(9), recvdname )

       if (Io.ne.my_tile_i0(bibj)) stop 'comprecv_r4tiles: bad Io'
       if (Jo.ne.my_tile_j0(bibj)) stop 'comprecv_r4tiles: bad Jo'
       if (nx.ne.my_tile_nx(bibj)) stop 'comprecv_r4tiles: bad nx'
       if (ny.ne.my_tile_ny(bibj)) stop 'comprecv_r4tiles: bad ny'
       if (recvdname .ne. dataname) then
        write(LogUnit,*) 'comprecv_r4tiles: recvdname = ',recvdname
        write(LogUnit,*) 'comprecv_r4tiles:  dataname = ',dataname
        stop 'comprecv_r4tiles: recvdname != dataname'
       endif

! Copy buffer to interior of tile
       k=1
       do j=1,Nj
        do i=1,Ni
         ij=HEADER_SIZE+i+Ni*(j-1)
         arr(i,j,k,bi,bj)=r4buf(ij)
        enddo
       enddo

      enddo ! bibj

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
