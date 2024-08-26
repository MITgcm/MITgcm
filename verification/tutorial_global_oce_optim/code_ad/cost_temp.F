#include "COST_OPTIONS.h"
c#ifdef ALLOW_CTRL
c# include "CTRL_OPTIONS.h"
c#endif

CBOP
C     !ROUTINE: COST_TEMP
C     !INTERFACE:
      SUBROUTINE COST_TEMP( myThid )
C     *==========================================================*
C     | SUBROUTINE COST_TEMP
C     | o the subroutine computes the sum of the squared errors
C     |   relatively to the Levitus climatology
C     *==========================================================*
C
C     \ev
C     !USES:
       IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "cost.h"
#ifdef ALLOW_OPENAD
# include "FFIELDS.h"
#else
# include "cost_local.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid
CEOP

#ifdef ALLOW_COST_TEMP
C     Abuse this routine as driver to call a free-format function
      CALL COST_TEMP_FFORM(
     I     sNx, sNy, nSx, nSy, OLx, OLy, Nr,
     I     myByLo(myThid), myByHi(myThid),
     I     myBxLo(myThid), myBxHi(myThid),
     I     maskC, wtheta, cMeanTheta,
     O     objf_temp_tut,
     I     myThid )
#endif /* ALLOW_COST_TEMP */

      RETURN
      END
