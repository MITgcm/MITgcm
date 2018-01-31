!=======================================================================
      subroutine mitcplr_real2char( rarg, carg )
      implicit none
! Predefined constants/arrays
#include "CPLR_SIG.h"
! Arguments
      character*(MAXLEN_COMP_NAME) carg
      real*4 rarg(MAXLEN_COMP_NAME)
! Local
      integer j
!     ------------------------------------------------------------------

! Convert the integer array to characters
      do j=1,MAXLEN_COMP_NAME
       carg(j:j) = char( nint( rarg(j) ) )
      enddo

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
