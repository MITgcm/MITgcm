#include "CPP_EEOPTIONS.h"

      SUBROUTINE MDS_BYTESWAPR8( n, arr )
C IN:
C   n    INTEGER :: Number of 8-byte words in arr
C IN/OUT:
C   arr  real*8  :: Array declared as real*8(n)
C
C Created: 05/05/99 adcroft@mit.edu (This is an unfortunate hack!!)

      IMPLICIT NONE

C Arguments
      INTEGER n
      CHARACTER*(*) arr

C Local
      INTEGER i
      CHARACTER*(1) cc

      DO i=1,8*n,8
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
      ENDDO

      RETURN
      END
