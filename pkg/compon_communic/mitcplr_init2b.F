C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/mitcplr_init2b.F,v 1.3 2013/11/27 21:48:30 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine mitcplr_init2b( myTypeStr )

      implicit none
! Predefined constants/arrays
#include "CPLR_SIG.h"
! MPI variables
#include "mpif.h"

! Arguments
      character*(*) myTypeStr
! Local
      integer myid, numprocs, ierr
      integer MPI_COMM_temp
      integer n,j,lenbuf,compind
      integer ibuf(MAX_IBUF)
!     ------------------------------------------------------------------

! Foreach component type
      do compind = 1,num_components

       MPI_COMM_temp=MPI_COMM_compcplr(compind)

! Find-out my position (rank) in the "global" communicator
       call MPI_COMM_RANK( MPI_COMM_temp, myid, ierr )
       if (ierr.ne.0) write(LogUnit,*) 'MITCPLR_init2b: ',
     &     ' Rank = ',myid,' MPI_COMM_RANK ierr=',ierr
! How big is the "global" comminicator?
       call MPI_COMM_SIZE( MPI_COMM_temp, numprocs, ierr )
       if (ierr.ne.0) write(LogUnit,*) 'MITCPLR_init2b: ',
     &     ' Size = ',numprocs,' MPI_COMM_RANK ierr=',ierr
       if (DEBUG) write(LogUnit,*) 'MITCPLR_init2b: ',
     &     ' Rank/Size = ',myid,' /',numprocs

!     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

! Assume nothing again
       num_coupler_procs=0
       num_component_procs(compind)=0
       num_compcplr_procs(compind)=0

! Receive a message from each of the other processes in "myglobal"
       do n=0,numprocs-1
        ibuf( 1)=myid
        ibuf( 2)=MITCPLR_COUPLER
        ibuf( 3)=0
        ibuf( 4)=0
        ibuf( 5)=0
        ibuf( 6)=0
        ibuf( 7)=0
        ibuf( 8)=0
        ibuf( 9)=0
        ibuf(10)=0
        ibuf(11)=0
        ibuf(12)=0
        lenbuf=12
        call MPI_Bcast(
     &               ibuf, lenbuf, MPI_INTEGER,
     &               n,
     &               MPI_COMM_temp, ierr )
        if (ierr.ne.0) write(LogUnit,*) 'MITCPLR_init2b: ',
     &    ' MPI_Bcast from ',ibuf(1),ibuf(2),' ierr=',ierr

        if ( ibuf(2).eq.MITCPLR_COUPLER ) then
! If the broadcaster is the "coupler"
         num_coupler_procs=num_coupler_procs + 1
         rank_coupler_procs(num_coupler_procs) = ibuf(1)
         num_compcplr_procs(compind)=num_compcplr_procs(compind) + 1
         j=num_compcplr_procs(compind)
         rank_compcplr_procs(j,compind)=ibuf(1)
        else
! If the broadcaster is a "component"
         num_component_procs(compind)=num_component_procs(compind) + 1
         j=num_component_procs(compind)
         rank_component_procs(j,compind)=ibuf(1)
         num_compcplr_procs(compind)=num_compcplr_procs(compind) + 1
         j=num_compcplr_procs(compind)
         rank_compcplr_procs(j,compind)=ibuf(1)
        endif

       enddo

       if (num_coupler_procs .ne. 1) then
        STOP 'MITCPLR_init2b: I can only handle one coupling process'
       endif

      enddo

!     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      do compind = 1,num_components
       write(LogUnit,*) 'MITCPLR_init2b: ',
     &      MPI_COMM_compcplr(compind),
     &     ' comp. ranks = ',(rank_component_procs(j,compind),
     &                        j=1,num_component_procs(compind) )
       write(LogUnit,*) 'MITCPLR_init2b: ',
     &      MPI_COMM_compcplr(compind),
     &     ' c+c ranks = ',(rank_compcplr_procs(j,compind),
     &                        j=1,num_compcplr_procs(compind) )
      enddo

!     ------------------------------------------------------------------
      call flush(LogUnit)
      return
      end
!=======================================================================
