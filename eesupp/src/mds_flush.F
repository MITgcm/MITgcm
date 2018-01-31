#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: MDS_FLUSH

C     !INTERFACE:
      SUBROUTINE MDS_FLUSH( ioUnit, myThid )

C     !DESCRIPTION:
C     call the intrinsic subroutine FLUSH (if available) on the ioUnit argument

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
C     ioUnit (integer) :: unit number
C     myThid (integer) :: my Thread Id number
      INTEGER ioUnit
      INTEGER myThid

C     !LOCAL VARIABLES:
CEOP

#ifdef HAVE_FLUSH
      CALL FLUSH( ioUnit )
#endif

      RETURN
      END
