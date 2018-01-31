!=======================================================================
      subroutine mitcplr_char2dbl( carg, rarg )
      implicit none
! Predefined constants/arrays
#include "CPLR_SIG.h"
! Arguments
      character*(*) carg
      real*8 rarg(MAXLEN_COMP_NAME)
! Local
      integer j
      character*(MAXLEN_COMP_NAME) cbuftmp
!     ------------------------------------------------------------------

! Copy carg into a fixed length character array
      cbuftmp = carg

! Convert the character array to integers
      do j=1,MAXLEN_COMP_NAME
       rarg(j)= ichar( cbuftmp(j:j) )
      enddo

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
