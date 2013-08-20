#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: IFNBLNK

C     !INTERFACE:
      subroutine oad_s_IFNBLNK( string, index )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION IFNBLNK                                         |
C     | o Find first non-blank in character string.              |
C     *==========================================================*
C
C     !INPUT PARAMETERS:
C     string :: String to find first non-blank in.
      CHARACTER*(*) string
      integer index

C     !LOCAL VARIABLES:
C     L, LS :: Temps for string locations
      INTEGER L, LS
CEOP
C
      LS     = LEN(string)
      index = 0
      L=1
      DO while ((L.le.LS).and.(string(L:L) .eq. ' '))
        L=L+1
      end do
      index = L
      END
