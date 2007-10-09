C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/couprecv_r4.F,v 1.2 2007/10/08 23:58:20 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine couprecv_r4( component, dataname, Nx, Ny, arr )
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
      integer i,j,ij,n
      integer Ni,Io,Nj,Jo
      character*(MAXLEN_COMP_NAME) recvdname
!     ------------------------------------------------------------------

! Establish who I am communicating with
      compind=mitcplr_match_comp( component )
      if (compind.le.0) stop 'couprecv_r4: Bad component id'
      comm=MPI_COMM_compcplr( compind )
      numprocs=num_component_procs(compind)
      if (numprocs.lt.1) then
       write(LogUnit,*) 'couprecv_r4: compind = ',compind
       stop 'couprecv_r4: numprocs < 1'
      endif
      if (VERB)
     &  write(LogUnit,*) 'couprecv_r4: ',component_Name(compind)
      if (VERB)
     &  write(LogUnit,*) 'couprecv_r4: dataname=',dataname

! Foreach component process
      do n=1,numprocs

! Receive message
       count=HEADER_SIZE+MAX_R4_BUFLEN
       dtype=MPI_REAL
       tag=generate_tag(102,n,dataname)
       rank=rank_component_procs(n,compind)
       if (VERB) then
        write(LogUnit,*) 'couprecv_r4: calling MPI_Recv rank=',rank,
     &    ' proc=',n,'/',numprocs
        call flush(LogUnit)
       endif
       call MPI_Recv(r4buf, count, dtype, rank, tag, comm, stat, ierr)
       if (VERB) then
        write(LogUnit,*) 'couprecv_r4: returned ierr=',ierr
        call flush(LogUnit)
       endif

       if (ierr.ne.0) then
        write(LogUnit,*) 'couprecv_r4tiles: rank(W,G)=',
     &            my_rank_in_world,my_rank_in_global,
     &            ' ierr=',ierr
        stop 'couprecv_r4: MPI_Recv failed'
       endif

! Extract header
       Io=int(0.5+r4buf(1))
       Ni=int(0.5+r4buf(2))
       Jo=int(0.5+r4buf(3))
       Nj=int(0.5+r4buf(4))

       if (Io+Ni-1.gt.Nx .or. Io.lt.1) then
        write(LogUnit,*) 'couprecv_r4tiles: Io,Ni=',Io,Ni
        stop 'couprecv_r4: Incompatible header/target array'
       endif
       if (Jo+Nj-1.gt.Ny .or. Jo.lt.1) then
        write(LogUnit,*) 'couprecv_r4tiles: Jo,Nj=',Jo,Nj
        stop 'couprecv_r4: Incompatible header/target array'
       endif

       call mitcplr_real2char( r4buf(9), recvdname )
       if (recvdname .ne. dataname) then
        write(LogUnit,*) 'couprecv_r4: recvdname = ',recvdname
        write(LogUnit,*) 'couprecv_r4:  dataname = ',dataname
        stop 'couprecv_r4: recvdname != dataname'
       endif

! Extract data
       do j=1,Nj
        do i=1,Ni
         ij=HEADER_SIZE+i+Ni*(j-1)
         arr(Io+i-1,Jo+j-1)=r4buf(ij)
        enddo
       enddo

      enddo

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
