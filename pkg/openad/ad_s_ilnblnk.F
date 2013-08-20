#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: ILNBLNK

C     !INTERFACE:
      subroutine oad_s_ILNBLNK( string, index )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION ILNBLNK                                         |
C     | o Find last non-blank in character string.               |
C     *==========================================================*

C     !INPUT PARAMETERS:
C     string :: string to scan
      CHARACTER*(*) string
      integer index

C     !LOCAL VARIABLES:
C     L, LS :: Temps. used in scanning string
      INTEGER L, LS
CEOP
C
      LS      = LEN(string)
      index = 0
      L=LS
      DO while ((L.gt.0).and.(string(L:L) .eq. ' '))
        L=L-1
      end do
      index = L
      END
