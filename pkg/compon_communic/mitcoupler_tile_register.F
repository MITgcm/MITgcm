C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/mitcoupler_tile_register.F,v 1.3 2013/11/27 21:51:15 jmc Exp $
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
      external mitcplr_match_comp
      external generate_tag

! Local
      integer n,numprocs
      integer comm
      integer compind,count,dtype,tag,rank
      integer ierr
      integer stat(MPI_STATUS_SIZE)
      integer j, numtiles
      integer nx, ny, i0, j0
      integer ibuf(MAX_IBUF)

!     ------------------------------------------------------------------

      write(LogUnit,'(3A)')
     &    'MITCOUPLER_tile_register: do "', compName, '" :'

! Establish who I am communicating with
      compind=mitcplr_match_comp( compName )
      if (compind.le.0) STOP 'MITCOUPLER_tile_register: Bad component'
      comm=MPI_COMM_compcplr( compind )
      numprocs=num_component_procs(compind)

      write(LogUnit,'(2(A,I6))')
     &        ' compind=', compind, ' ; numprocs=', numprocs
      if (numprocs.lt.1) then
       STOP 'MITCOUPLER_tile_register: numprocs < 1'
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
        STOP 'MITCOUPLER_tile_register: MPI_Recv failed'
       endif

       numtiles=ibuf(1)
       if (numtiles.lt.1 .or. numtiles.gt.MAX_TILES) then
        write(LogUnit,*) 'MITCOUPLER_tile_register: #tiles = ',numtiles
        STOP 'MITCOUPLER_tile_register: invalid value for numtiles'
       endif
       component_num_tiles(n,compind)=numtiles

       write(LogUnit,'(3(A,I6),A)')   '- proc # =', n,
     &     ' ; rank=', rank, ' ; numtiles=', numtiles, '  -------'
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
         STOP 'MITCOUPLER_tile_register: MPI_Recv failed'
        endif

! Extract data and store
        nx = ibuf(1)
        ny = ibuf(2)
        i0 = ibuf(3)
        j0 = ibuf(4)
        component_tile_nx(j,n,compind) = nx
        component_tile_ny(j,n,compind) = ny
        component_tile_i0(j,n,compind) = i0
        component_tile_j0(j,n,compind) = j0

! Print and check
        write(LogUnit,'(A,I5,A,2I5,A,2I8)') ' tile #:', j,
     &     ' ; Ni,Nj=', nx, ny, ' ; Io,Jo=', i0, j0

        if (nx.lt.1) then
         STOP 'MITCOUPLER_tile_register: invalid value for nx'
        endif
        if (ny.lt.1) then
         STOP 'MITCOUPLER_tile_register: invalid value for ny'
        endif
        if (i0.lt.1) then
         STOP 'MITCOUPLER_tile_register: invalid value for i0'
        endif
        if (j0.lt.1) then
         STOP 'MITCOUPLER_tile_register: invalid value for j0'
        endif
        if (i0+nx-1.gt.nnx) then
         STOP 'MITCOUPLER_tile_register: i0 + nx -1 > nnx'
        endif
        if (j0+ny-1.gt.nny) then
         STOP 'MITCOUPLER_tile_register: j0 + ny -1 > nny'
        endif

       enddo ! j
       write(LogUnit,'(A,2I8,2(A,I8))')
     &     ' rank(W,G)=', my_rank_in_world, my_rank_in_global,
     &     ' , rank = ',rank, ' , num_tiles = ', numtiles

      enddo ! n

      write(LogUnit,'(3A)') 'MITCOUPLER_tile_register: comp. "',
     &                         compName, '" done'

!     ------------------------------------------------------------------
      call flush(LogUnit)
      return
      end
!=======================================================================
