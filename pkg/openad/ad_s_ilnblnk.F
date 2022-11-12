#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: ILNBLNK

C     !INTERFACE:
      SUBROUTINE oad_s_ILNBLNK( string, index )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION ILNBLNK                                         |
C     | o Find last non-blank in character string.               |
C     *==========================================================*

C     !INPUT PARAMETERS:
C     string :: string to scan
      CHARACTER*(*) string
      INTEGER index

C     !LOCAL VARIABLES:
C     L  :: Temp. used in scanning string
      INTEGER L
CEOP

      index = 0
      L = LEN(string)
      DO WHILE ( L .GT. 0 )
        IF ( string(L:L) .NE. ' ' ) THEN
          index = L
          L = 0
        ELSE
          L = L - 1
        ENDIF
      ENDDO
      END
