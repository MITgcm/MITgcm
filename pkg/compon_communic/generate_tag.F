C $Header: /u/gcmpack/MITgcm/pkg/compon_communic/generate_tag.F,v 1.3 2014/10/23 16:05:21 jmc Exp $
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
c     integer j,hash,pow
c     integer itrans(MAXLEN_COMP_NAME)
!     ------------------------------------------------------------------

! Convert the characters to an integer array
c     call mitcplr_char2int( carg, itrans )

! Hash the integer array
c     hash=1
c     pow=1
c     do j=1,MAXLEN_COMP_NAME
c      hash=abs(hash+itrans(j)*pow)
c      pow=mod(pow*128,256*256)
c     enddo

c     generate_tag=(hash*256+iarg2)*128+iarg1
c     generate_tag=abs(mod(generate_tag,256*256*256))
      generate_tag = iarg1*MAX_TILES + iarg2

c     if (DEBUG)
c    & write(0,*) 'generate_tag = ',generate_tag,iarg1,iarg2,carg

!     ------------------------------------------------------------------
      return
      end
!=======================================================================
