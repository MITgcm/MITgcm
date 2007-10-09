C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/mitcoupler_init.F,v 1.2 2007/10/08 23:58:20 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine MITCOUPLER_init( myTypeStr, MPI_COMM_local )
      implicit none

! Arguments
      character*(*) myTypeStr
      integer MPI_COMM_local

! MPI variables
#include "mpif.h"
      integer myid, numprocs, ierr, rc

! Predefined constants/arrays
#include "CPLR_SIG.h"

! Functions
      integer mitcplr_match_comp

! Local
      integer n,j
      integer MPI_GROUP_World
      integer MPI_GROUP_Tmp
      integer lenbuf
      integer compind
      character*(MAXLEN_COMP_NAME) cbuf
      logical couplerFlag

!     ------------------------------------------------------------------

! Homogeneous registration phase (every process Bcasts)
      couplerFlag=.TRUE.
      call MITCPLR_init1( myTypeStr, couplerFlag )

! Return the communicator for my component
      MPI_COMM_local=MPI_COMM_mylocal

! Heterogeneous registration phase (within component incl. coupler)
      call mitcplr_init2b( myTypeStr )

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
