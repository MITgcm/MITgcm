!=======================================================================
      subroutine mitcplr_int2char( iarg, carg )
      implicit none
! Predefined constants/arrays
#include "CPLR_SIG.h"
! Arguments
      character*(MAXLEN_COMP_NAME) carg
      integer iarg(MAXLEN_COMP_NAME)
! Local
      integer j
!     ------------------------------------------------------------------

! Convert the integer array to characters
      do j=1,MAXLEN_COMP_NAME
       carg(j:j) = char( iarg(j) )
      enddo

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
