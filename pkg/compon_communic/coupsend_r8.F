C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/coupsend_r8.F,v 1.3 2009/09/14 16:22:03 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine coupsend_r8( component, dataname, Nx, Ny, arr )
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
      integer count,dtype,dest,tag,comm,ierr
! Functions
      integer mitcplr_match_comp
      integer generate_tag
! Local
      integer compind,numprocs
      integer i,j,ij,n
      integer Ni,Io,Nj,Jo
!     ------------------------------------------------------------------

! Establish who I am communicating with
      compind=mitcplr_match_comp( component )
      if (compind.le.0) stop 'coupsend_r8: Bad component id'
      comm=MPI_COMM_compcplr( compind )
      numprocs=num_component_procs(compind)
      if (numprocs.lt.1) then
       write(LogUnit,*) 'coupsend_r8: compind = ',compind
       stop 'coupsend_r8: numprocs < 1'
      endif
      if (VERB)
     &  write(LogUnit,*) 'coupsend_r8: ',component_Name(compind)
      if (VERB)
     &  write(LogUnit,*) 'coupsend_r8: dataname=',dataname

! Foreach component process
      do n=1,numprocs

! Create header
       Io=component_tile_i0(1,n,compind)
       Jo=component_tile_j0(1,n,compind)
       Ni=component_tile_nx(1,n,compind)
       Nj=component_tile_ny(1,n,compind)
       r8buf(1)=float( Io )
       r8buf(2)=float( Jo )
       r8buf(3)=float( Ni )
       r8buf(4)=float( Nj )
       call mitcplr_char2dbl( dataname, r8buf(9) )

! Pack data
       do j=1,Nj
        do i=1,Ni
         ij=HEADER_SIZE+i+Ni*(j-1)
         r8buf(ij)=arr(Io+i-1,Jo+j-1)
        enddo
       enddo

! Send message
       count=HEADER_SIZE+Ni*Nj
       dtype=MPI_DOUBLE_PRECISION
       tag=generate_tag(121,n,dataname)
       dest=rank_component_procs(n,compind)

       if (VERB) then
        write(LogUnit,*) 'coupsend_r8: calling MPI_Send dest=',dest,
     &    ' proc=',n,'/',numprocs
        call flush(LogUnit)
       endif
       call MPI_Send( r8buf, count, dtype, dest, tag, comm, ierr )
       if (VERB) then
        write(LogUnit,*) 'coupsend_r8: returned ierr=',ierr
        call flush(LogUnit)
       endif

       if (ierr.ne.0) then
        write(LogUnit,*) 'coupsend_r8tiles: rank(W,G)=',
     &            my_rank_in_world,my_rank_in_global,
     &            ' ierr=',ierr
        stop 'coupsend_r8: MPI_Recv failed'
       endif

      enddo ! n

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
