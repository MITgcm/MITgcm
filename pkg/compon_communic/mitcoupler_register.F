C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/mitcoupler_register.F,v 1.2 2007/10/08 23:58:20 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine MITCOUPLER_register( compName, nnx, nny )
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
      integer j,numtiles,nx,ny,i0,j0

!     ------------------------------------------------------------------

! Establish who I am communicating with
      compind=mitcplr_match_comp( compName )
      if (compind.le.0) stop 'MITCOUPLER_register: Bad component'
      comm=MPI_COMM_compcplr( compind )
      numprocs=num_component_procs(compind)
      if (numprocs.lt.1) then
       write(LogUnit,*) 'MITCOUPLER_register: compind = ',compind
       stop 'MITCOUPLER_register: numprocs < 1'
      endif

! Foreach component process
      do n=1,numprocs

! Receive message
       count=MAX_IBUF
       dtype=MPI_INTEGER
       tag=generate_tag(115,n,'Register')
       rank=rank_component_procs(n,compind)

       call MPI_Recv(ibuf, count, dtype, rank, tag, comm, stat, ierr)

       if (ierr.ne.0) then
        write(LogUnit,*) 'MITCOUPLER_register: rank(W,G)=',
     &            my_rank_in_world,my_rank_in_global,
     &            ' ierr=',ierr
        stop 'MITCOUPLER_register: MPI_Recv failed'
       endif

! Extract data
       numtiles=ibuf(1)
       nx=ibuf(2)
       ny=ibuf(3)
       i0=ibuf(4)
       j0=ibuf(5)

       if (numtiles.ne.1) then
        write(LogUnit,*) 'MITCOUPLER_tile_register: #tiles = ',numtiles
        stop 'MITCOUPLER_tile_register: invalid value for numtiles'
       endif
       if (nx.lt.1) then
        write(LogUnit,*) 'MITCOUPLER_register: nx = ',nx
        stop 'MITCOUPLER_register: invalid value for nx'
       endif
       if (ny.lt.1) then
        write(LogUnit,*) 'MITCOUPLER_register: ny = ',ny
        stop 'MITCOUPLER_register: invalid value for ny'
       endif
       if (i0.lt.1) then
        write(LogUnit,*) 'MITCOUPLER_register: i0 = ',i0
        stop 'MITCOUPLER_register: invalid value for i0'
       endif
       if (j0.lt.1) then
        write(LogUnit,*) 'MITCOUPLER_register: j0 = ',j0
        stop 'MITCOUPLER_register: invalid value for j0'
       endif
       if (i0+nx-1.gt.nnx) then
        write(LogUnit,*) 'MITCOUPLER_register: i0 = ',i0
        stop 'MITCOUPLER_register: i0 + nx -1 > nnx'
       endif
       if (j0+ny-1.gt.nny) then
        write(LogUnit,*) 'MITCOUPLER_register: j0 = ',j0
        stop 'MITCOUPLER_register: j0 + ny -1 > nny'
       endif

       component_num_tiles(n,compind)=1
       component_tile_nx(1,n,compind)=nx
       component_tile_ny(1,n,compind)=ny
       component_tile_i0(1,n,compind)=i0
       component_tile_j0(1,n,compind)=j0

      enddo ! n

      do n=1,numprocs
       write(LogUnit,*) 'MITCOUPLER_register: proc,nx,ny = ',n,
     &    component_tile_nx(1,n,compind),component_tile_ny(1,n,compind)
      enddo ! n

!     ------------------------------------------------------------------
      call flush(LogUnit)
      return
      end
!=======================================================================
