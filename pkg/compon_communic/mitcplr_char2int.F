C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/mitcplr_char2int.F,v 1.2 2007/10/08 23:58:21 jmc Exp $
C $Name:  $

!=======================================================================
      subroutine mitcplr_char2int( carg, iarg )
      implicit none
! Predefined constants/arrays
#include "CPLR_SIG.h"
! Arguments
      character*(*) carg
      integer iarg(MAXLEN_COMP_NAME)
! Local
      integer j
      character*(MAXLEN_COMP_NAME) cbuftmp
!     ------------------------------------------------------------------

! Copy carg into a fixed length character array
      cbuftmp = carg

! Convert the character array to integers
      do j=1,MAXLEN_COMP_NAME
       iarg(j)=ichar( cbuftmp(j:j) )
      enddo

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
