C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/mds_byteswap.F,v 1.4 2010/08/24 13:40:38 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

C--  File mds_byteswap.F: Routines to do IO byte swapping
C--   Contents
C--   o MDS_BYTESWAPR4
C--   o MDS_BYTESWAPR8

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDS_BYTESWAPR4( n, arr )
C IN:
C   n          integer :: Number of 4-byte words in arr
C IN/OUT:
C   arr        real*4  :: Array declared as real*4(n)
C
C Created: 05/05/99 adcroft@mit.edu (This is an unfortunate hack!!)

      IMPLICIT NONE
C Arguments
      INTEGER n
      CHARACTER*(*) arr

#ifdef _BYTESWAPIO

C Local
      integer i
      character*(1) cc
C     ------------------------------------------------------------------
      do i=1,4*n,4
       cc=arr(i:i)
       arr(i:i)=arr(i+3:i+3)
       arr(i+3:i+3)=cc
       cc=arr(i+1:i+1)
       arr(i+1:i+1)=arr(i+2:i+2)
       arr(i+2:i+2)=cc
      enddo
C     ------------------------------------------------------------------
#endif /* _BYTESWAPIO */
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDS_BYTESWAPR8( n, arr )
C IN:
C   n          integer :: Number of 8-byte words in arr
C IN/OUT:
C   arr        real*8  :: Array declared as real*8(n)
C
C Created: 05/05/99 adcroft@mit.edu (This is an unfortunate hack!!)

      IMPLICIT NONE
C Arguments
      INTEGER n
      CHARACTER*(*) arr

#ifdef _BYTESWAPIO

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
#endif /* _BYTESWAPIO */
      RETURN
      END
