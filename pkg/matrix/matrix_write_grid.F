#include "MATRIX_OPTIONS.h"

      SUBROUTINE MATRIX_WRITE_GRID( myThid )

      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

      INTEGER myThid

#ifdef ALLOW_MATRIX

C     local variables:

      IF (useMatrix) THEN
        CALL WRITE_FLD_XY_RS( 'DXF',' ',DXF,0,myThid)
        CALL WRITE_FLD_XY_RS( 'DYF',' ',DYF,0,myThid)
      ENDIF

#endif /* ALLOW_MATRIX */
      RETURN
      END
