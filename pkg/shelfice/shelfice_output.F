#include "SHELFICE_OPTIONS.h"

CBOP
C     !ROUTINE: SHELFICE_OUTPUT
C     !INTERFACE:
      SUBROUTINE SHELFICE_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_OUTPUT
C     | o Do SHELFICE snapshot output
C     |   and compute & write time-average output.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SHELFICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: my time in simulation ( s )
C     myIter :: my Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(10) suff
#ifdef ALLOW_MNC
      CHARACTER*(1) pf
#endif
CEOP

      IF (SHELFICEwriteState) THEN

C- note: surf.Fluxes have not yet been computed when called @ nIter0
       IF ( myIter .NE. nIter0 .AND.
     &      DIFFERENT_MULTIPLE(SHELFICE_dumpFreq,myTime,deltaTClock)
     &    ) THEN

#ifdef ALLOW_MNC
        IF ( useMNC .AND. SHELFICE_dump_mnc ) THEN
         IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
           pf(1:1) = 'D'
         ELSE
           pf(1:1) = 'R'
         ENDIF
         CALL MNC_CW_SET_UDIM('shelfice', -1, myThid)
         CALL MNC_CW_RL_W_S('D','shelfice',0,0,'T', myTime, myThid)
         CALL MNC_CW_SET_UDIM('shelfice', 0, myThid)
         CALL MNC_CW_I_W_S('I','shelfice',0,0,'iter', myIter, myThid)
         CALL MNC_CW_RL_W_S('D','shelfice',0,0,'model_time',
     &        myTime,myThid)
         CALL MNC_CW_RS_W(pf,'shelfice',0,0,'fwFlux',
     &        shelfIceFreshWaterFlux,myThid)
         CALL MNC_CW_RS_W(pf,'shelfice',0,0,'heatFlux',
     &        shelfIceHeatFlux,myThid)
        ENDIF
#endif
        IF (SHELFICE_dump_mdsio) THEN
         IF ( rwSuffixType.EQ.0 ) THEN
           WRITE(suff,'(I10.10)') myIter
         ELSE
           CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
         ENDIF
         CALL WRITE_FLD_XY_RS( 'SHICE_fwFlux.',suff,
     &        shelficeFreshWaterFlux,myIter,myThid)
         CALL WRITE_FLD_XY_RS( 'SHICE_heatFlux.',suff,
     &        shelficeHeatFlux,myIter,myThid)
        ENDIF

       ENDIF
      ENDIF

      RETURN
      END
