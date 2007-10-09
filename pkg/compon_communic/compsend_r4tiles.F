C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/compsend_r4tiles.F,v 1.2 2007/10/08 23:58:20 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine compsend_r4tiles( dataname,Ni,Oi,Nj,Oj,Nk,Ti,Tj, arr )
      implicit none
! Arguments
      character*(*) dataname
      integer Ni,Oi,Nj,Oj,Nk,Ti,Tj
      real*4 arr(1-Oi:Ni+Oi,1-Oj:Nj+Oj,Nk,Ti,Tj)
! Predefined constants/arrays
#include "CPLR_SIG.h"
! MPI variables
#include "mpif.h"
      integer count,datatype,dest,tag,comm,ierr
! Functions
      integer generate_tag
! Local
      integer i,j,ij,bi,bj,k,bibj
!     ------------------------------------------------------------------

      if (HEADER_SIZE+Ni*Nj.gt.MAX_R4_BUFLEN)
     &    stop 'compsend_r4tiles: Nx*Ny too big'

! Foreach tile which is non-blank
      do bibj=1,my_num_tiles

       bi=my_tile_bi(bibj)
       bj=my_tile_bj(bibj)

       if (Ni.ne.my_tile_nx(bibj))
     &     stop 'compsend_r4tiles: ni != my_tile_nx'
       if (Nj.ne.my_tile_ny(bibj))
     &     stop 'compsend_r4tiles: nj != my_tile_ny'

! Set up buffer
       r4buf(1)=float( my_tile_i0(bibj) )
       r4buf(2)=float( my_tile_nx(bibj) )
       r4buf(3)=float( my_tile_j0(bibj) )
       r4buf(4)=float( my_tile_ny(bibj) )
       call mitcplr_char2real( dataname, r4buf(9) )

! Copy interior of tile to buffer
       k=1
       do j=1,Nj
        do i=1,Ni
         ij=HEADER_SIZE+i+Ni*(j-1)
         r4buf(ij)=arr(i,j,k,bi,bj)
        enddo
       enddo

! Send message
       count=HEADER_SIZE+Ni*Nj
       datatype=MPI_REAL
       dest=my_coupler_rank
       tag=generate_tag(103,bibj,dataname)
       comm=MPI_COMM_myglobal

       if (VERB) then
        write(LogUnit,*)
     &   'compsend_r4tiles: calling MPI_Send dest=',dest,' tile=',bibj
        write(LogUnit,*) 'compsend_r4tiles: dataname=',dataname
        call flush(LogUnit)
       endif
       call MPI_Send( r4buf, count, datatype, dest, tag, comm, ierr )
       if (VERB) then
        write(LogUnit,*) 'compsend_r4tiles: returned ierr=',ierr
        call flush(LogUnit)
       endif

       if (ierr.ne.0) then
        write(LogUnit,*) 'compsend_r4tiles: rank(W,G,L)=',
     &            my_rank_in_world,my_rank_in_global,my_rank_in_local,
     &            ' ierr=',ierr
        stop 'compsend_r4tiles: MPI_Send failed'
       endif

      enddo ! bibj

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
