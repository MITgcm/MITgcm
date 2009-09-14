C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/couprecv_r8tiles.F,v 1.3 2009/09/14 16:22:03 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine couprecv_r8tiles( component, dataname, Nx, Ny, arr )
      implicit none
! Arguments
      character*(*) component
      character*(*) dataname
      integer Nx,Ny
      real*8 arr(Nx,Ny)
! Predefined constants/arrays
#include "CPLR_SIG.h"
! MPI variables
#include "mpif.h"
      integer count,dtype,rank,tag,comm,ierr
      integer stat(MPI_STATUS_SIZE)
! Functions
      integer mitcplr_match_comp
      integer generate_tag
! Local
      integer compind,numprocs
      integer i,j,ij,n,bibj
      integer Ni,Io,Nj,Jo
      character*(MAXLEN_COMP_NAME) recvdname
!     ------------------------------------------------------------------

! Establish who I am communicating with
      compind=mitcplr_match_comp( component )
      if (compind.le.0) stop 'couprecv_r8tiles: Bad component id'
      comm=MPI_COMM_compcplr( compind )
      numprocs=num_component_procs(compind)
      if (numprocs.lt.1) then
       write(LogUnit,*) 'couprecv_r8tiles: compind = ',compind
       stop 'couprecv_r8tiles: numprocs < 1'
      endif
      if (VERB)
     &  write(LogUnit,*) 'couprecv_r8tiles: ',component_Name(compind)
      if (VERB)
     &  write(LogUnit,*) 'couprecv_r8tiles: dataname=',dataname

! Foreach component process
      do n=1,numprocs

! Foreach tile within process
       do bibj=1,component_num_tiles(n,compind)

! Receive message
       count=MAX_R8_BUFLEN
       dtype=MPI_DOUBLE_PRECISION
       tag=generate_tag(103,bibj,dataname)
       rank=rank_component_procs(n,compind)

       if (VERB) then
        write(LogUnit,*)
     &    'couprecv_r8tiles: calling MPI_Recv rank=',rank,
     &    ' proc=',n,'/',numprocs,' tile=',bibj
        call flush(LogUnit)
       endif
       call MPI_Recv(r8buf, count, dtype, rank, tag, comm, stat, ierr)
       if (VERB) then
        write(LogUnit,*) 'couprecv_r8tiles: returned ierr=',ierr
        call flush(LogUnit)
       endif

       if (ierr.ne.0) then
        write(LogUnit,*) 'couprecv_r8tiles: rank(W,G)=',
     &            my_rank_in_world,my_rank_in_global,
     &            ' ierr=',ierr
        stop 'couprecv_r8tiles: MPI_Recv failed'
       endif

! Extract header
       Io=int(0.5+r8buf(1))
       Ni=int(0.5+r8buf(2))
       Jo=int(0.5+r8buf(3))
       Nj=int(0.5+r8buf(4))

       if (Io+Ni-1.gt.Nx .or. Io.lt.1) then
        write(LogUnit,*) 'couprecv_r8tiles: Io,Ni=',Io,Ni
        stop 'couprecv_r8tiles: Incompatible header/target array'
       endif
       if (Jo+Nj-1.gt.Ny .or. Jo.lt.1) then
        write(LogUnit,*) 'couprecv_r8tiles: Jo,Nj=',Jo,Nj
        stop 'couprecv_r8tiles: Incompatible header/target array'
       endif
       if (Io.ne.component_tile_i0(bibj,n,compind))
     &    stop 'couprecv_r8tiles: Io != component_tile_i0'
       if (Jo.ne.component_tile_j0(bibj,n,compind))
     &    stop 'couprecv_r8tiles: Jo != component_tile_j0'
       if (Ni.ne.component_tile_nx(bibj,n,compind))
     &    stop 'couprecv_r8tiles: Ni != component_tile_nx'
       if (Nj.ne.component_tile_ny(bibj,n,compind))
     &    stop 'couprecv_r8tiles: Nj != component_tile_ny'

       call mitcplr_dbl2char( r8buf(9), recvdname )
       if (recvdname .ne. dataname) then
        write(LogUnit,*) 'couprecv_r8tiles: recvdname = ',recvdname
        write(LogUnit,*) 'couprecv_r8tiles:  dataname = ',dataname
        stop 'couprecv_r8tiles: recvdname != dataname'
       endif

! Extract data
       do j=1,Nj
        do i=1,Ni
         ij=HEADER_SIZE+i+Ni*(j-1)
         arr(Io+i-1,Jo+j-1)=r8buf(ij)
        enddo
       enddo

       enddo ! bibj

      enddo ! n

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
