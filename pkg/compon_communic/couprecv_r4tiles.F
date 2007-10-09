C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/couprecv_r4tiles.F,v 1.2 2007/10/08 23:58:20 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine couprecv_r4tiles( component, dataname, Nx, Ny, arr )
      implicit none
! Arguments
      character*(*) component
      character*(*) dataname
      integer Nx,Ny
      real*4 arr(Nx,Ny)
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
      if (compind.le.0) stop 'couprecv_r4tiles: Bad component id'
      comm=MPI_COMM_compcplr( compind )
      numprocs=num_component_procs(compind)
      if (numprocs.lt.1) then
       write(LogUnit,*) 'couprecv_r4tiles: compind = ',compind
       stop 'couprecv_r4tiles: numprocs < 1'
      endif
      if (VERB)
     &  write(LogUnit,*) 'couprecv_r4tiles: ',component_Name(compind)
      if (VERB)
     &  write(LogUnit,*) 'couprecv_r4tiles: dataname=',dataname

! Foreach component process
      do n=1,numprocs

! Foreach tile within process
       do bibj=1,component_num_tiles(n,compind)

! Receive message
       count=HEADER_SIZE+MAX_R4_BUFLEN
       dtype=MPI_REAL
       tag=generate_tag(103,bibj,dataname)
       rank=rank_component_procs(n,compind)

       if (VERB) then
        write(LogUnit,*)
     &    'couprecv_r4tiles: calling MPI_Recv rank=',rank,
     &    ' proc=',n,'/',numprocs,' tile=',bibj
        call flush(LogUnit)
       endif
       call MPI_Recv(r4buf, count, dtype, rank, tag, comm, stat, ierr)
       if (VERB) then
        write(LogUnit,*) 'couprecv_r4tiles: returned ierr=',ierr
        call flush(LogUnit)
       endif

       if (ierr.ne.0) then
        write(LogUnit,*) 'couprecv_r4tiles: rank(W,G)=',
     &            my_rank_in_world,my_rank_in_global,
     &            ' ierr=',ierr
        stop 'couprecv_r4tiles: MPI_Recv failed'
       endif

! Extract header
       Io=int(0.5+r4buf(1))
       Ni=int(0.5+r4buf(2))
       Jo=int(0.5+r4buf(3))
       Nj=int(0.5+r4buf(4))

       if (Io+Ni-1.gt.Nx .or. Io.lt.1) then
        write(LogUnit,*) 'couprecv_r4tiles: Io,Ni=',Io,Ni
        stop 'couprecv_r4tiles: Incompatible header/target array'
       endif
       if (Jo+Nj-1.gt.Ny .or. Jo.lt.1) then
        write(LogUnit,*) 'couprecv_r4tiles: Jo,Nj=',Jo,Nj
        stop 'couprecv_r4tiles: Incompatible header/target array'
       endif
       if (Io.ne.component_tile_i0(bibj,n,compind))
     &    stop 'couprecv_r4tiles: Io != component_tile_i0'
       if (Jo.ne.component_tile_j0(bibj,n,compind))
     &    stop 'couprecv_r4tiles: Jo != component_tile_j0'
       if (Ni.ne.component_tile_nx(bibj,n,compind))
     &    stop 'couprecv_r4tiles: Ni != component_tile_nx'
       if (Nj.ne.component_tile_ny(bibj,n,compind))
     &    stop 'couprecv_r4tiles: Nj != component_tile_ny'

       call mitcplr_real2char( r4buf(9), recvdname )
       if (recvdname .ne. dataname) then
        write(LogUnit,*) 'couprecv_r4tiles: recvdname = ',recvdname
        write(LogUnit,*) 'couprecv_r4tiles:  dataname = ',dataname
        stop 'couprecv_r4tiles: recvdname != dataname'
       endif

! Extract data
       do j=1,Nj
        do i=1,Ni
         ij=HEADER_SIZE+i+Ni*(j-1)
         arr(Io+i-1,Jo+j-1)=r4buf(ij)
        enddo
       enddo

       enddo ! bibj

      enddo ! n

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
