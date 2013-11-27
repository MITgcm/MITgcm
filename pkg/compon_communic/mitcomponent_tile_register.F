C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/mitcomponent_tile_register.F,v 1.3 2013/11/27 21:53:04 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine MITCOMPONENT_tile_register( num_tiles, iReg )
      implicit none

! MPI variables
#include "mpif.h"
c     integer myid, numprocs, ierr, rc

! Predefined constants/arrays
#include "CPLR_SIG.h"

! Arguments
      integer num_tiles
      integer iReg(6,num_tiles)

! Functions
      integer mitcplr_match_comp
      integer generate_tag
      external mitcplr_match_comp
      external generate_tag

! Local
c     integer bi, bj
      integer n
      integer count, datatype, dest, tag, comm, ierr
      integer ibuf(MAX_IBUF)

!     ------------------------------------------------------------------

      write(LogUnit,'(A,I6,A,2I4)')
     &  'MITCOMPONENT_tile_register (pId=', my_rank_in_local,
     &  '): Starts ; num_tiles=', num_tiles

      if (num_tiles.lt.1)
     & STOP 'MITCOMPONENT_tile_register: num_tiles < 1'
      if (num_tiles.gt.MAX_TILES)
     & STOP 'MITCOMPONENT_tile_register: num_tiles > MAX_TILES'

      my_num_tiles = num_tiles
      do n=1,num_tiles
        my_tile_bi(n) = iReg(1,n)
        my_tile_bj(n) = iReg(2,n)
        my_tile_nx(n) = iReg(3,n)
        my_tile_ny(n) = iReg(4,n)
        my_tile_i0(n) = iReg(5,n)
        my_tile_j0(n) = iReg(6,n)
        write(LogUnit,'(A,I5,A,2I4,A,2I5,A,2I8)')
     &    ' tile #', n,
     &    ' ; bi,bj=', iReg(1,n), iReg(2,n),
     &    ' ; Ni,Nj=', iReg(3,n), iReg(4,n),
     &    ' ; Io,Jo=', iReg(5,n), iReg(6,n)
      enddo

! Set up buffer
      ibuf(1) = num_tiles

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
       STOP 'MITCOMPONENT_tile_register: MPI_Send failed'
      endif

      do n=1,my_num_tiles

! Set up buffer
c      bi = my_tile_bi(n)
c      bj = my_tile_bj(n)
       ibuf(1) = my_tile_nx(n)
       ibuf(2) = my_tile_ny(n)
       ibuf(3) = my_tile_i0(n)
       ibuf(4) = my_tile_j0(n)

! Send message
       count=4
       datatype=MPI_INTEGER
       dest=my_coupler_rank
       tag=generate_tag(113,n,'Register each tile')
       comm=MPI_COMM_myglobal

       call MPI_Send( ibuf, count, datatype, dest, tag, comm, ierr )

       if (ierr.ne.0) then
        write(LogUnit,*) 'MITCOMPONENT_tile_register: rank(W,G,L)=',
     &           my_rank_in_world,my_rank_in_global,my_rank_in_local,
     &           ' ierr=',ierr
        STOP 'MITCOMPONENT_tile_register: MPI_Send failed'
       endif

      enddo

      write(LogUnit,'(A,I6,A,2I4)')
     &  'MITCOMPONENT_tile_register (pId=', my_rank_in_local, '): done'

!     ------------------------------------------------------------------
      call flush(LogUnit)
      return
      end
!=======================================================================
