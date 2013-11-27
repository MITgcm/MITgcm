C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/mitcplr_init1.F,v 1.3 2013/11/27 21:48:30 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine MITCPLR_init1( myTypeStr, couplerFlag )
      implicit none

! MPI variables
#include "mpif.h"

! Predefined constants/arrays
#include "CPLR_SIG.h"

! Arguments
      character*(*) myTypeStr
      logical couplerFlag

! Functions
      integer mitcplr_match_comp
      external mitcplr_match_comp

! Local
      integer myid, numprocs, ierr
      integer n,j
      integer MPI_GROUP_World
      integer MPI_GROUP_Tmp
      integer lenbuf
      integer compind
      integer ibuf(MAX_IBUF)
      character*(MAXLEN_COMP_NAME) cbuf

!     ------------------------------------------------------------------

! Find-out my position (rank) in the "world" communicator
      call MPI_COMM_RANK( MPI_COMM_WORLD, myid, ierr )
      if (ierr.ne.0) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &     ' Rank = ',myid,' MPI_COMM_RANK ierr=',ierr
! How big is the "world"?
      call MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr )
      if (ierr.ne.0) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &     ' Size = ',numprocs,' MPI_COMM_RANK ierr=',ierr
      if (DEBUG) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &     ' Rank/Size = ',myid,' /',numprocs

!     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

! Registration: collect/bcast lists of who is who

! Assume nothing
      num_components = 0
      num_coupler_procs = 0

! Receive a message from each of the other processes
      do n=0,numprocs-1
       ibuf(1)=myid
       ibuf(2)=0
       if ( couplerFlag ) ibuf(2)=MITCPLR_COUPLER
       ibuf(3)=0
       ibuf(4)=0
       ibuf(5)=0
       ibuf(6)=0
       ibuf(7)=0
       call mitcplr_char2int( myTypeStr, ibuf(8) )
       lenbuf=8+MAXLEN_COMP_NAME
       call MPI_Bcast(
     &               ibuf, lenbuf, MPI_INTEGER,
     &               n,
     &               MPI_COMM_WORLD, ierr )
       if (DEBUG) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &    ' MPI_Bcast from ',ibuf(1),ibuf(2),' ierr=',ierr
       call mitcplr_int2char( ibuf(8), cbuf )
       if (DEBUG) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &    ' Bcast cbuf=',cbuf,'x'

       if ( ibuf(2).eq.MITCPLR_COUPLER ) then
! If the broadcaster is the "coupler"
        num_coupler_procs=num_coupler_procs + 1
        rank_coupler_procs(num_coupler_procs) = ibuf(1)
        coupler_Name=cbuf
       else
! If the broadcaster is a "component"
        compind=mitcplr_match_comp( cbuf )
        if (DEBUG) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &    ' compind=',compind
        num_component_procs(compind)=num_component_procs(compind) + 1
        j=num_component_procs(compind)
        rank_component_procs(j,compind)=ibuf(1)
       endif

      enddo

      if ( num_coupler_procs .ne. 1 ) then
       STOP 'MITCPLR_init1: I can only handle one coupler process'
      endif

      do compind=1,num_components
       num_compcplr_procs(compind)=num_component_procs(compind) + 1
       do j=1,num_compcplr_procs(compind)
        rank_compcplr_procs(j,compind)=rank_component_procs(j,compind)
       enddo
       j=num_compcplr_procs(compind)
       rank_compcplr_procs(j,compind)=rank_coupler_procs(1)
       call mitcplr_sortranks( j, rank_compcplr_procs(1,compind) )
      enddo

      if (DEBUG) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &   ' coupler=',coupler_Name,
     &   ( rank_coupler_procs(j),j=1,num_coupler_procs )
      do n=1,num_components
       if (DEBUG) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &    ' component=',component_Name(n),
     &    ( rank_component_procs(j,n),j=1,num_component_procs(n) )
       if (DEBUG) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &    ' comp+cplr=',component_Name(n),
     &    ( rank_compcplr_procs(j,n),j=1,num_compcplr_procs(n) )
      enddo

!     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

! Create new groups and communicators

! Establish MPI_GROUP_World associated with MPI_COMM_WORLD
      call MPI_Comm_group( MPI_COMM_WORLD, MPI_GROUP_World, ierr )
      if (DEBUG) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &    ' MPI_Comm_group MPI_GROUP_World=',MPI_GROUP_World,
     &    ' ierr=',ierr

      do n=1,num_components

! Create group MPI_GROUP_Tmp
       call MPI_Group_incl(
     &   MPI_GROUP_World,
     &   num_component_procs(n),
     &   rank_component_procs(1,n),
     &   MPI_GROUP_Tmp,
     &   ierr )
       if (DEBUG) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &   ' MPI_Group_incl MPI_GROUP_Tmp=',
     &   MPI_GROUP_Tmp,' ierr=',ierr

! Create communicator MPI_COMM_component
       call MPI_Comm_create(
     &   MPI_COMM_WORLD,
     &   MPI_GROUP_Tmp,
     &   MPI_COMM_component(n),
     &   ierr )
       if (DEBUG) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &   ' MPI_Comm_create MPI_COMM_component=',MPI_COMM_component(n),
     &   ' ierr=',ierr

! Create group MPI_GROUP_Tmp
       call MPI_Group_incl(
     &   MPI_GROUP_World,
     &   num_compcplr_procs(n),
     &   rank_compcplr_procs(1,n),
     &   MPI_GROUP_Tmp,
     &   ierr )
       if (DEBUG) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &   ' MPI_Group_incl MPI_GROUP_Tmp=',
     &   MPI_GROUP_Tmp,' ierr=',ierr

! Create communicator MPI_COMM_compcplr
       call MPI_Comm_create(
     &   MPI_COMM_WORLD,
     &   MPI_GROUP_Tmp,
     &   MPI_COMM_compcplr(n),
     &   ierr )
       if (DEBUG) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &   ' MPI_Comm_create MPI_COMM_compcplr=',MPI_COMM_compcplr(n),
     &   ' ierr=',ierr

      enddo

!     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      if ( couplerFlag ) then
       my_component_ind=-1
       MPI_COMM_mylocal=MPI_COMM_World
       MPI_COMM_myglobal=MPI_COMM_World
       my_component_name=coupler_Name
      else
       compind=mitcplr_match_comp( myTypeStr )
       my_component_ind=compind
       MPI_COMM_mylocal=MPI_COMM_component( compind )
       MPI_COMM_myglobal=MPI_COMM_compcplr( compind )
       my_component_name=component_Name( compind )
      endif

      if (DEBUG) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &   ' component num=',compind,
     &   ' MPI_COMM=',MPI_COMM_mylocal,MPI_COMM_myglobal

      if ( couplerFlag ) then
       do n=1,num_components
! Find-out my position (rank) in the "global" communicator
        call MPI_COMM_RANK( MPI_COMM_compcplr(n), j, ierr )
        if (ierr.ne.0) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &     ' Rank = ',j,' MPI_COMM_RANK ierr=',ierr
! How big is the "global" communicator?
        call MPI_COMM_SIZE( MPI_COMM_compcplr(n), numprocs, ierr )
        if (ierr.ne.0) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &     ' Size = ',numprocs,' MPI_COMM_SIZE ierr=',ierr
        if (DEBUG) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &     ' Rank/Size = ',j,' /',numprocs,
     &     ' in Component =',n
       enddo
      else
! Find-out my position (rank) in the "global" communicator
       call MPI_COMM_RANK( MPI_COMM_myglobal, j, ierr )
       if (ierr.ne.0) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &     ' Rank = ',j,' MPI_COMM_RANK ierr=',ierr
! How big is the "global" communicator?
       call MPI_COMM_SIZE( MPI_COMM_myglobal, numprocs, ierr )
       if (ierr.ne.0) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &     ' Size = ',numprocs,' MPI_COMM_SIZE ierr=',ierr
       if (DEBUG) write(0,*) 'MITCPLR_init1: ',myid,' ',myTypeStr,
     &     ' Rank/Size = ',j,' /',numprocs
      endif

! Open log file
      if ( couplerFlag ) j=myid
      write(cbuf(1:MAXLEN_COMP_NAME),'(2a,i4.4,a)')
     &   myTypeStr,'.',j,'.clog'
      open(LogUnit,file=cbuf,status='unknown',form='formatted')
      write(LogUnit,'(2a)') '========================================',
     & '========================================'
      write(LogUnit,*) 'This is "',myTypeStr,'"'
      write(LogUnit,*) 'myid in MPI_COMM_World = ',myid
      if (.not.couplerFlag)
     &  write(LogUnit,*) 'myid in MPI_COMM_Global = ',j

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
