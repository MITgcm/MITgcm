C $Header: /u/gcmpack/MITgcm/eesupp/src/mds_byteswapi4.F,v 1.3 2009/01/09 22:51:13 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

      subroutine MDS_BYTESWAPI4( n, arr )
C IN:
C   n      integer    :: Number of 4-byte words in arr
C IN/OUT:
C   arr    integer*4  :: Array declared as integer*4(n)
C
C Created: 05/05/99 adcroft@mit.edu (This is an unfortunate hack!!)

      implicit none

#ifdef FAST_BYTESWAP

C Arguments
      integer n
      integer(kind=4) arr(n), i32

C Local
      integer i

      i32(i) = ishft(i.and. z'ff000000', -24) .or.
     &         ishft(i.and. z'00ff0000',  -8) .or.
     &         ishft(i.and. z'0000ff00',   8) .or.
     &         ishft(i.and. z'000000ff',  24)
      do i = 1,n
      arr(i) = i32(arr(i))
      enddo

#else /* FAST_BYTESWAP */

C Arguments
      integer n
      character*(*) arr

C Local
      integer i
      character*(1) cc

      do i=1,4*n,4
       cc=arr(i:i)
       arr(i:i)=arr(i+3:i+3)
       arr(i+3:i+3)=cc
       cc=arr(i+1:i+1)
       arr(i+1:i+1)=arr(i+2:i+2)
       arr(i+2:i+2)=cc
      enddo

#endif /* FAST_BYTESWAP */

      return
      end
