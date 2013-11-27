C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/mitcplr_init2a.F,v 1.3 2013/11/27 21:48:30 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine mitcplr_init2a( myTypeStr )

      implicit none
! Predefined constants/arrays
#include "CPLR_SIG.h"
! MPI variables
#include "mpif.h"

! Arguments
      character*(*) myTypeStr
! Local
      integer myid, numprocs, ierr
      integer n,j,lenbuf
      integer ibuf(MAX_IBUF)
!     ------------------------------------------------------------------

! Find-out my position (rank) in the "global" communicator
      call MPI_COMM_RANK( MPI_COMM_myglobal, myid, ierr )
      if (ierr.ne.0) write(LogUnit,*) 'MITCPLR_init2a: ',
     &     ' Rank = ',myid,' MPI_COMM_RANK ierr=',ierr
! How big is the "global" comminicator?
      call MPI_COMM_SIZE( MPI_COMM_myglobal, numprocs, ierr )
      if (ierr.ne.0) write(LogUnit,*) 'MITCPLR_init2a: ',
     &     ' Size = ',numprocs,' MPI_COMM_RANK ierr=',ierr
      if (VERB) write(LogUnit,*) 'MITCPLR_init2a: ',
     &     ' Rank/Size = ',myid,' /',numprocs

!     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

! Assume nothing again
      num_coupler_procs=0

! Receive a message from each of the other processes in "myglobal"
      do n=0,numprocs-1
       ibuf( 1)=myid
       ibuf( 2)=0
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
     &               MPI_COMM_myglobal, ierr )
       if (ierr.ne.0) write(LogUnit,*) 'MITCPLR_init2a: ',
     &    ' MPI_Bcast from ',ibuf(1),ibuf(2),' ierr=',ierr

       if ( ibuf(2).eq.MITCPLR_COUPLER ) then
! If the broadcaster is the "coupler"
        num_coupler_procs=num_coupler_procs + 1
        num_procs_in_global=num_procs_in_global+1
        rank_coupler_procs(num_coupler_procs) = ibuf(1)
       else
! If the broadcaster is a "component"
        num_procs_in_local=num_procs_in_local+1
        num_procs_in_global=num_procs_in_global+1
       endif

      enddo

      if (num_coupler_procs .ne. 1) then
       STOP 'MITCPLR_init2a: I can only handle one coupling process'
      endif

!     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

! Find-out my position (rank) in the "world" communicator
      call MPI_COMM_RANK( MPI_COMM_World, n, ierr )
      if (ierr.ne.0) write(LogUnit,*) 'MITCPLR_init2a: ',
     &     ' Rank = ',myid,' MPI_COMM_RANK ierr=',ierr
! Find-out my position (rank) in the "local" communicator
      call MPI_COMM_RANK( MPI_COMM_myglobal, j, ierr )
      if (ierr.ne.0) write(LogUnit,*) 'MITCPLR_init2a: ',
     &     ' Rank = ',myid,' MPI_COMM_RANK ierr=',ierr

! Store the information
      my_coupler_rank=rank_coupler_procs(1)
      my_rank_in_world=n
      my_rank_in_global=myid
      my_rank_in_local=j

      if (VERB) write(LogUnit,*) 'MITCPLR_init2a: ',
     &     ' rank (W,G,L) =',
     &     my_rank_in_world,my_rank_in_global,my_rank_in_local,
     &     ' my_coupler_rank =',my_coupler_rank,' /',numprocs

!     ------------------------------------------------------------------
      call flush(LogUnit)
      return
      end
!=======================================================================
