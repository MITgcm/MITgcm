C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/generate_tag.F,v 1.2 2007/10/08 23:58:20 jmc Exp $
C $Name:  $

!=======================================================================
      integer function generate_tag( iarg1, iarg2, carg )
      implicit none
! Predefined constants/arrays
#include "CPLR_SIG.h"
! Arguments
      integer iarg1
      integer iarg2
      character*(*) carg
! Local
      integer j,hash,pow
      integer itrans(MAXLEN_COMP_NAME)
!     ------------------------------------------------------------------

! Convert the characters to an integer array
      call mitcplr_char2int( carg, itrans )

! Hash the integer array
      hash=1
      pow=1
      do j=1,MAXLEN_COMP_NAME
       hash=abs(hash+itrans(j)*pow)
       pow=mod(pow*128,256*256)
      enddo

      generate_tag=(hash*256+iarg2)*128+iarg1
      generate_tag=abs(mod(generate_tag,256*256*256))

c     if (DEBUG)
c    & write(0,*) 'generate_tag = ',generate_tag,iarg1,iarg2,carg

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
