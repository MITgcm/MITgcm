#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE DIC_INI_FORCING( myThid )

C     /==========================================================\
C     | SUBROUTINE DIC_INI_FORCING                               |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PTRACERS.h"
#include "GCHEM.h"
#include "DIC_ABIOTIC.h"
#ifdef DIC_BIOTIC
#include "DIC_BIOTIC.h"
#include "DIC_LOAD.h"
#endif

C     == Routine arguments ==
      INTEGER myThid

#ifdef ALLOW_PTRACERS
C     == Local variables ==
c
C First call requires that we initialize everything to zero for safety
      CALL LEF_ZERO( wspeed0,myThid )
      CALL LEF_ZERO( wspeed1,myThid )
      CALL LEF_ZERO( atmosp0,myThid )
      CALL LEF_ZERO( atmosp1,myThid )
      CALL LEF_ZERO( silica0,myThid )
      CALL LEF_ZERO( silica1,myThid )
      CALL LEF_ZERO( ice0,myThid )
      CALL LEF_ZERO( ice1,myThid )
# ifdef ALLOW_FE
      CALL LEF_ZERO( feinput0,myThid )
      CALL LEF_ZERO( feinput1,myThid )
# endif

       _EXCH_XY_R4(wspeed0, myThid )
       _EXCH_XY_R4(wspeed1, myThid )
       _EXCH_XY_R4(atmosp0, myThid )
       _EXCH_XY_R4(atmosp1, myThid )
       _EXCH_XY_R4(silica0, myThid )
       _EXCH_XY_R4(silica1, myThid )
       _EXCH_XY_R4(ice0, myThid )
       _EXCH_XY_R4(ice1, myThid )
# ifdef ALLOW_FE
       _EXCH_XY_R4(feinput0, myThid )
       _EXCH_XY_R4(feinput1, myThid )
# endif

#endif
      RETURN
      END
