#include "CPP_EEOPTIONS.h"

      SUBROUTINE MDS_BYTESWAPR4( n, arr )
C IN:
C   n    INTEGER :: Number of 4-byte words in arr
C IN/OUT:
C   arr  real*4  :: Array declared as real*4(n)
C
C Created: 05/05/99 adcroft@mit.edu (This is an unfortunate hack!!)

      IMPLICIT NONE

C Arguments
      INTEGER n
      CHARACTER*(*) arr

C Local
      INTEGER i
      CHARACTER*(1) cc

      DO i=1,4*n,4
       cc=arr(i:i)
       arr(i:i)=arr(i+3:i+3)
       arr(i+3:i+3)=cc
       cc=arr(i+1:i+1)
       arr(i+1:i+1)=arr(i+2:i+2)
       arr(i+2:i+2)=cc
      ENDDO

      RETURN
      END
