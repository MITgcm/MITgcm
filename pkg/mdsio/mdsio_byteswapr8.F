C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_byteswapr8.F,v 1.1 2001/03/06 15:28:53 adcroft Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

      subroutine MDS_BYTESWAPR8( n, arr )
C IN:
C   n		integer - Number of 8-byte words in arr
C IN/OUT:
C   arr		real*8  - Array declared as real*4(n)
C
C Created: 05/05/99 adcroft@mit.edu (This is an unfortunate hack!!)

      implicit none
C Arguments
      integer n
      character*(*) arr
C Local
      integer i
      character*(1) cc
C     ------------------------------------------------------------------
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
C     ------------------------------------------------------------------
      return
      end
