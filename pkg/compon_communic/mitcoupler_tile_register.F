C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/mitcoupler_tile_register.F,v 1.2 2007/10/08 23:58:20 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine MITCOUPLER_tile_register( compName, nnx, nny )
      implicit none

! Arguments
      character*(*) compName
      integer nnx, nny

! MPI variables
#include "mpif.h"

! Predefined constants/arrays
#include "CPLR_SIG.h"

! Functions
      integer mitcplr_match_comp
      integer generate_tag

! Local
      integer n,numprocs
      integer comm
      integer compind,count,dtype,tag,rank
      integer ierr, rc
      integer stat(MPI_STATUS_SIZE)
      integer j,numtiles

!     ------------------------------------------------------------------

! Establish who I am communicating with
      compind=mitcplr_match_comp( compName )
      if (compind.le.0) stop 'MITCOUPLER_tile_register: Bad component'
      comm=MPI_COMM_compcplr( compind )
      numprocs=num_component_procs(compind)
      if (numprocs.lt.1) then
       write(LogUnit,*) 'MITCOUPLER_tile_register: compind = ',compind
       stop 'MITCOUPLER_tile_register: numprocs < 1'
      endif

! Foreach component process
      do n=1,numprocs

! Receive message
       count=MAX_IBUF
       dtype=MPI_INTEGER
       tag=generate_tag(112,n,'Register Tiles')
       rank=rank_component_procs(n,compind)

       call MPI_Recv(ibuf, count, dtype, rank, tag, comm, stat, ierr)

       if (ierr.ne.0) then
        write(LogUnit,*) 'MITCOUPLER_tile_register: rank(W,G)=',
     &            my_rank_in_world,my_rank_in_global,
     &            ' ierr=',ierr
        stop 'MITCOUPLER_tile_register: MPI_Recv failed'
       endif

       numtiles=ibuf(1)
       if (numtiles.lt.1 .or. numtiles.gt.MAX_TILES) then
        write(LogUnit,*) 'MITCOUPLER_tile_register: #tiles = ',numtiles
        stop 'MITCOUPLER_tile_register: invalid value for numtiles'
       endif
       component_num_tiles(n,compind)=numtiles

       do j=1,numtiles

! Receive message
        count=MAX_IBUF
        dtype=MPI_INTEGER
        tag=generate_tag(113,j,'Register each tile')
        rank=rank_component_procs(n,compind)

        call MPI_Recv(ibuf, count, dtype, rank, tag, comm, stat, ierr)

        if (ierr.ne.0) then
         write(LogUnit,*) 'MITCOUPLER_tile_register: rank(W,G)=',
     &            my_rank_in_world,my_rank_in_global,
     &            ' ierr=',ierr
         stop 'MITCOUPLER_tile_register: MPI_Recv failed'
        endif

        component_tile_nx(j,n,compind)=ibuf(1)
        component_tile_ny(j,n,compind)=ibuf(2)
        component_tile_i0(j,n,compind)=ibuf(3)
        component_tile_j0(j,n,compind)=ibuf(4)

        if (VERB) then
         write(LogUnit,*) 'MITCOUPLER_tile_register:',
     &             my_rank_in_world,my_rank_in_global,
     &   ' proc,tile,nx,ny,i0,j0 = ',n,j,ibuf(1),ibuf(2),ibuf(3),ibuf(4)
        endif

       enddo ! j
       write(LogUnit,*) 'MITCOUPLER_tile_register:',
     &             my_rank_in_world,my_rank_in_global,
     &            ' rank = ',rank,
     &            ' num_tiles = ',numtiles

      enddo ! n

!     ------------------------------------------------------------------
      call flush(LogUnit)
      return
      end
!=======================================================================
