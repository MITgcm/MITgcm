C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/mitcplr_char2real.F,v 1.2 2007/10/08 23:58:21 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine mitcplr_char2real( carg, rarg )
      implicit none
! Predefined constants/arrays
#include "CPLR_SIG.h"
! Arguments
      character*(*) carg
      real*4 rarg(MAXLEN_COMP_NAME)
! Local
      integer j
      character*(MAXLEN_COMP_NAME) cbuftmp
!     ------------------------------------------------------------------

! Copy carg into a fixed length character array
      cbuftmp = carg

! Convert the character array to integers
      do j=1,MAXLEN_COMP_NAME
       rarg(j)=float( ichar( cbuftmp(j:j) ) )
      enddo

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
