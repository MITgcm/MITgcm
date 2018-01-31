#include "DIC_OPTIONS.h"

CStartOfInterface
      SUBROUTINE DIC_STORE_FLUXCO2(  myTime,
     I                               myIter,
     I                               myThid )
C     *==========================================================*
C     | SUBROUTINE DIC_STORE_FLUXCO2                             |
C     | o Routine for controlling storage of air-sea CO2 flux to |
C     |   to the coupling layer.                                 |
C     |==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT   |
C     | Coupler "checkpoint1" library calls.                     |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C     == DIC variables
#include "DIC_VARS.h"
#ifdef COMPONENT_MODULE
C     == Global variables for coupling interface ==
# include "CPL_PARAMS.h"
# include "OCNCPL.h"
#endif

C     == Routine arguments ==
C     myThid :: Thread number for this instance of the routine
C     myIter :: Current timestep number
C     myTime :: Current model time
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef COMPONENT_MODULE
C     == Local variables ==
C     i,j    :: Loop counters
C     bi,bj  :: Tile index
      INTEGER i,j,bi,bj

      IF ( ocn_cplExch_DIC ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
            fluxCO2cpl(i,j,bi,bj) = fluxCO2(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
