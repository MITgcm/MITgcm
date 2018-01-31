!=======================================================================
      subroutine MITCOUPLER_init( 
     I                      myTypeStr, 
     O                      MPI_COMM_local, msgUnit )
      implicit none

! MPI variables
#include "mpif.h"

! Predefined constants/arrays
#include "CPLR_SIG.h"

! Arguments
      character*(*) myTypeStr
      integer MPI_COMM_local
      integer msgUnit

! Functions

! Local
      logical couplerFlag

!     ------------------------------------------------------------------

! Homogeneous registration phase (every process Bcasts)
!  also open a coupling log file (.clog) with I/O unit: LogUnit
      couplerFlag = .TRUE.
      call MITCPLR_init1( myTypeStr, couplerFlag )
      msgUnit = LogUnit

! Return the communicator for my component
      MPI_COMM_local = MPI_COMM_mylocal

! Heterogeneous registration phase (within component incl. coupler)
      call mitcplr_init2b( myTypeStr )

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
