C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/couprecv_i4vec.F,v 1.1 2013/12/02 21:32:39 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine couprecv_i4vec( component, dataname, length, vecFld )
      implicit none
! Predefined constants/arrays
#include "CPLR_SIG.h"
! MPI variables
#include "mpif.h"
! Arguments
      character*(*) component
      character*(*) dataname
      integer length
      integer vecFld(length)
! Functions
      integer mitcplr_match_comp
      integer generate_tag
      external mitcplr_match_comp
      external generate_tag
! Local
      integer count,dtype,rank,tag,comm,ierr
      integer stat(MPI_STATUS_SIZE)
      integer compind, numprocs
      integer i, j, n, ndiff
      integer ibuf(MAX_IBUF)
!     ------------------------------------------------------------------

      if ( 1+length .gt. MAX_IBUF )
     &    STOP 'couprecv_i4vec: length exceeds MAX_IBUF'

! Establish who I am communicating with
      compind = mitcplr_match_comp( component )
      if (compind.le.0) STOP 'couprecv_i4vec: Bad component id'
      comm = MPI_COMM_compcplr( compind )
      numprocs = num_component_procs(compind)
      if (numprocs.lt.1) then
        write(LogUnit,*) 'couprecv_i4vec: compind = ',compind
        STOP 'couprecv_i4vec: numprocs < 1'
      endif
      if (VERB)
     &  write(LogUnit,*) 'couprecv_i4vec: ',component_Name(compind)
      if (VERB)
     &  write(LogUnit,*) 'couprecv_i4vec: dataname=',dataname

! Foreach component process
      do n=1,numprocs

! Receive message
       count = MAX_IBUF
       dtype = MPI_INTEGER
       tag = generate_tag( 115, n, dataname)
       rank = rank_component_procs(n,compind)

       if (VERB) then
        write(LogUnit,*)
     &    'couprecv_i4vec: calling MPI_Recv rank=',rank,
     &    ' proc=',n,'/',numprocs
        call flush(LogUnit)
       endif
       call MPI_Recv(ibuf, count, dtype, rank, tag, comm, stat, ierr)
       if (VERB) then
        write(LogUnit,*) 'couprecv_i4vec: returned ierr=',ierr
        call flush(LogUnit)
       endif

       if (ierr.ne.0) then
        write(LogUnit,*) 'couprecv_i4vec: rank(W,G)=',
     &            my_rank_in_world,my_rank_in_global,
     &            ' ierr=',ierr
        STOP 'couprecv_i4vec: MPI_Recv failed'
       endif

! Check header
       j = ibuf(1)

       if ( j.ne.length ) then
        write(LogUnit,*) 'couprecv_i4vec: length,header=', length, j
        STOP 'couprecv_i4vec: Incompatible header'
       endif

! Extract data
       if ( n.eq.1 ) then
         do i=1,length
           vecFld(i) = ibuf(i+1)
         enddo
       else
         ndiff = 0
         do i=1,length
           if  ( vecFld(i) .ne. ibuf(i+1) ) ndiff = ndiff + 1
         enddo
         if ( ndiff.gt.0 ) then
           write(LogUnit,'(A,I8,2A)')
     &      ' couprecv_i4vec: length=', length, ' name=', dataname
           write(LogUnit,'(A,I6,A,I8,A)')
     &      ' from proc=', n ,' : found', ndiff, ' differences (vs 1)'
         endif
       endif

      enddo ! n

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
