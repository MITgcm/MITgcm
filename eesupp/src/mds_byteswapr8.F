C $Header: /u/gcmpack/MITgcm/eesupp/src/mds_byteswapr8.F,v 1.3 2009/01/09 22:51:13 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

      subroutine MDS_BYTESWAPR8( n, arr )
C IN:
C   n    integer :: Number of 8-byte words in arr
C IN/OUT:
C   arr  real*8  :: Array declared as real*8(n)
C
C Created: 05/05/99 adcroft@mit.edu (This is an unfortunate hack!!)

      implicit none

#ifdef FAST_BYTESWAP

C Arguments
      integer n
      integer(kind=8) arr(n),i64,i1

C Local
      integer i

      i64(i1) = ishft(i1 .and. z'ff00000000000000',-56).or.
     &          ishft(i1 .and. z'00ff000000000000',-40).or.
     &          ishft(i1 .and. z'0000ff0000000000',-24).or.
     &          ishft(i1 .and. z'000000ff00000000', -8).or.
     &          ishft(i1 .and. z'00000000ff000000',  8).or.
     &          ishft(i1 .and. z'0000000000ff0000', 24).or.
     &          ishft(i1 .and. z'000000000000ff00', 40).or.
     &          ishft(i1 .and. z'00000000000000ff', 56)
      do i=1,n
      arr(i) = i64(arr(i))
      enddo

#else /* FAST_BYTESWAP */

C Arguments
      integer n
      character*(*) arr

C Local
      integer i
      character*(1) cc

      do i=1,8*n,8
       cc=arr(i:i)
       arr(i:i)=arr(i+7:i+7)
       arr(i+7:i+7)=cc
       cc=arr(i+1:i+1)
       arr(i+1:i+1)=arr(i+6:i+6)
       arr(i+6:i+6)=cc
       cc=arr(i+2:i+2)
       arr(i+2:i+2)=arr(i+5:i+5)
       arr(i+5:i+5)=cc
       cc=arr(i+3:i+3)
       arr(i+3:i+3)=arr(i+4:i+4)
       arr(i+4:i+4)=cc
      enddo

#endif /* FAST_BYTESWAP */

      return
      end
