C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/mitcomponent_tile_register.F,v 1.2 2007/10/08 23:58:20 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine MITCOMPONENT_tile_register( ntx, nty, ireg, rreg )
      implicit none

! Arguments
      integer ntx,nty
      integer ireg(4,ntx,nty)
      real*4  rreg(4,ntx,nty)

! MPI variables
#include "mpif.h"
      integer myid, numprocs, ierr, rc

! Predefined constants/arrays
#include "CPLR_SIG.h"

! Functions
      integer mitcplr_match_comp
      integer generate_tag

! Local
      integer bi,bj,num_tiles,bibj
      integer count,datatype,dest,tag,comm

!     ------------------------------------------------------------------

      num_tiles=0
      do bj=1,nty
       do bi=1,ntx
        if ( ireg(1,bi,bj)*ireg(2,bi,bj).gt.0 ) then
         num_tiles=num_tiles+1
         my_tile_nx(num_tiles)=ireg(1,bi,bj)
         my_tile_ny(num_tiles)=ireg(2,bi,bj)
         my_tile_i0(num_tiles)=ireg(3,bi,bj)
         my_tile_j0(num_tiles)=ireg(4,bi,bj)
         my_tile_bi(num_tiles)=bi
         my_tile_bj(num_tiles)=bj
         if (VERB)
     &   write(LogUnit,*) 'MITCOMPONENT_tile_register: bi,bj=',bi,bj,
     &              ' nx,ny=',ireg(1,bi,bj),ireg(2,bi,bj)
        else
         if (VERB) write(LogUnit,*)
     &    'MITCOMPONENT_tile_register: blank bi,bj=',bi,bj
        endif
       enddo
      enddo
      my_num_tiles=num_tiles
      if (VERB) write(LogUnit,*)
     &  'MITCOMPONENT_tile_register: num_tiles =',num_tiles

      if (num_tiles.lt.1)
     & stop 'MITCOMPONENT_tile_register: num_tiles < 1'
      if (num_tiles.gt.MAX_TILES)
     & stop 'MITCOMPONENT_tile_register: num_tiles > MAX_TILES'

! Set up buffer
      ibuf(1)=num_tiles

! Send message
      count=1
      datatype=MPI_INTEGER
      dest=my_coupler_rank
      tag=generate_tag(112,my_rank_in_global,'Register Tiles')
      comm=MPI_COMM_myglobal

      call MPI_Send( ibuf, count, datatype, dest, tag, comm, ierr )

      if (ierr.ne.0) then
       write(LogUnit,*) 'MITCOMPONENT_tile_register: rank(W,G,L)=',
     &            my_rank_in_world,my_rank_in_global,my_rank_in_local,
     &            ' ierr=',ierr
       stop 'MITCOMPONENT_tile_register: MPI_Send failed'
      endif

      do bibj=1,my_num_tiles

! Set up buffer
       bi=my_tile_bi(bibj)
       bj=my_tile_bj(bibj)
       ibuf(1)=my_tile_nx(bibj)
       ibuf(2)=my_tile_ny(bibj)
       ibuf(3)=my_tile_i0(bibj)
       ibuf(4)=my_tile_j0(bibj)

! Send message
       count=4
       datatype=MPI_INTEGER
       dest=my_coupler_rank
       tag=generate_tag(113,bibj,'Register each tile')
       comm=MPI_COMM_myglobal

       call MPI_Send( ibuf, count, datatype, dest, tag, comm, ierr )

       if (ierr.ne.0) then
        write(LogUnit,*) 'MITCOMPONENT_tile_register: rank(W,G,L)=',
     &           my_rank_in_world,my_rank_in_global,my_rank_in_local,
     &           ' ierr=',ierr
        stop 'MITCOMPONENT_tile_register: MPI_Send failed'
       endif

      enddo

!     ------------------------------------------------------------------
      call flush(LogUnit)
      return
      end
!=======================================================================
