C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/coupsend_r4tiles.F,v 1.2 2007/10/08 23:58:20 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine coupsend_r4tiles( component, dataname, Nx, Ny, arr )
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
      integer count,dtype,dest,tag,comm,ierr
! Functions
      integer mitcplr_match_comp
      integer generate_tag
! Local
      integer compind,numprocs
      integer i,j,ij,n,bibj
      integer Ni,Io,Nj,Jo
!     ------------------------------------------------------------------

! Establish who I am communicating with
      compind=mitcplr_match_comp( component )
      if (compind.le.0) stop 'coupsend_r4tiles: Bad component id'
      comm=MPI_COMM_compcplr( compind )
      numprocs=num_component_procs(compind)
      if (numprocs.lt.1) then
       write(LogUnit,*) 'coupsend_r4tiles: compind = ',compind
       stop 'coupsend_r4tiles: numprocs < 1'
      endif
      if (VERB)
     &  write(LogUnit,*) 'coupsend_r4tiles: ',component_Name(compind)
      if (VERB)
     &  write(LogUnit,*) 'coupsend_r4tiles: dataname=',dataname

! Foreach component process
      do n=1,numprocs

! Foreach tile on that process
       do bibj=1,component_num_tiles(n,compind)

! Create header
        Io=component_tile_i0(bibj,n,compind)
        Jo=component_tile_j0(bibj,n,compind)
        Ni=component_tile_nx(bibj,n,compind)
        Nj=component_tile_ny(bibj,n,compind)
        r4buf(1)=float( Io )
        r4buf(2)=float( Jo )
        r4buf(3)=float( Ni )
        r4buf(4)=float( Nj )
        call mitcplr_char2real( dataname, r4buf(9) )

! Pack data
        do j=1,Nj
         do i=1,Ni
          ij=HEADER_SIZE+i+Ni*(j-1)
          r4buf(ij)=arr(Io+i-1,Jo+j-1)
         enddo
        enddo

! Send message
        count=HEADER_SIZE+Ni*Nj
        dtype=MPI_REAL
        tag=generate_tag(123,bibj,dataname)
        dest=rank_component_procs(n,compind)

        if (VERB) then
         write(LogUnit,*) 'coupsend_r4tiles: calling MPI_Send dest=',
     &     dest,' proc=',n,'/',numprocs,' tile=',bibj
         call flush(LogUnit)
        endif
        call MPI_Send( r4buf, count, dtype, dest, tag, comm, ierr )
        if (VERB) then
         write(LogUnit,*) 'coupsend_r4tiles: returned ierr=',ierr
         call flush(LogUnit)
        endif

        if (ierr.ne.0) then
         write(LogUnit,*) 'coupsend_r4tiles: rank(W,G)=',
     &            my_rank_in_world,my_rank_in_global,
     &            ' ierr=',ierr
         stop 'coupsend_r4tiles: MPI_Recv failed'
        endif

       enddo ! bibj

      enddo ! n

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
