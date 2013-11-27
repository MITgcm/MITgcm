C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/mitcomponent_register.F,v 1.3 2013/11/27 21:48:30 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine MITCOMPONENT_register( nx, ny, i0, j0 )

      implicit none
! MPI variables
#include "mpif.h"

! Predefined constants/arrays
#include "CPLR_SIG.h"

! Arguments
      integer nx,ny,i0,j0

! Functions
      integer mitcplr_match_comp
      integer generate_tag
      external mitcplr_match_comp
      external generate_tag

! Local
      integer ierr
      integer count,datatype,dest,tag,comm
      integer ibuf(MAX_IBUF)

!     ------------------------------------------------------------------

! Set up buffer
      ibuf(1)=1
      ibuf(2)=nx
      ibuf(3)=ny
      ibuf(4)=i0
      ibuf(5)=j0

      my_num_tiles=1
      my_tile_nx(1)=nx
      my_tile_ny(1)=ny
      my_tile_i0(1)=i0
      my_tile_j0(1)=j0

! Send message
      count=5
      datatype=MPI_INTEGER
      dest=my_coupler_rank
      tag=generate_tag(115,my_rank_in_global,'Register')
      comm=MPI_COMM_myglobal

      call MPI_Send( ibuf, count, datatype, dest, tag, comm, ierr )

      if (ierr.ne.0) then
       write(LogUnit,*) 'MITCOMPONENT_register: rank(W,G,L)=',
     &            my_rank_in_world,my_rank_in_global,my_rank_in_local,
     &            ' ierr=',ierr
       STOP 'MITCOMPONENT_register: MPI_Send failed'
      endif

!     ------------------------------------------------------------------
      call flush(LogUnit)
      return
      end
!=======================================================================
