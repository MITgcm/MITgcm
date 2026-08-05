#include "GMREDI_OPTIONS.h"

CBOP
C     !ROUTINE: GMREDI_OUTPUT

C     !INTERFACE:
      SUBROUTINE GMREDI_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_OUTPUT
C     | o general routine for GM/Redi output
C     *==========================================================*
C     |   write time-average & snap-shot output
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"

C     !INPUT PARAMETERS:
C     myTime :: Current time of simulation ( s )
C     myIter :: Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_GMREDI
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
      CHARACTER*(10) suff
#ifdef ALLOW_MNC
      CHARACTER*(1) pf
#endif

      IF ( DIFFERENT_MULTIPLE(diagFreq,myTime,deltaTClock)
     &   ) THEN

        IF ( GM_MDSIO ) THEN
          IF ( rwSuffixType.EQ.0 ) THEN
            WRITE(suff,'(I10.10)') myIter
          ELSE
            CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
          ENDIF
          CALL WRITE_FLD_XYZ_RL( 'GM_Kux.',suff,Kux,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL( 'GM_Kvy.',suff,Kvy,myIter,myThid)
#ifdef GM_EXTRA_DIAGONAL
          IF (GM_ExtraDiag) THEN
            CALL WRITE_FLD_XYZ_RL( 'GM_Kuz.',suff,Kuz,myIter,myThid)
            CALL WRITE_FLD_XYZ_RL( 'GM_Kvz.',suff,Kvz,myIter,myThid)
          ENDIF
#endif
        ENDIF

#ifdef ALLOW_MNC
        IF ( GM_MNC ) THEN
          IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
           pf(1:1) = 'D'
          ELSE
           pf(1:1) = 'R'
          ENDIF
          CALL MNC_CW_SET_UDIM('gm_inst', -1, myThid)
          CALL MNC_CW_RL_W_S('D','gm_inst',0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM('gm_inst', 0, myThid)
          CALL MNC_CW_I_W_S('I','gm_inst',0,0,'iter',myIter,myThid)
          CALL MNC_CW_RL_W(pf,'gm_inst',0,0,'Kux',Kux,myThid)
          CALL MNC_CW_RL_W(pf,'gm_inst',0,0,'Kvy',Kvy,myThid)
#ifdef GM_EXTRA_DIAGONAL
          IF (GM_ExtraDiag) THEN
            CALL MNC_CW_RL_W(pf,'gm_inst',0,0,'Kuz',Kuz,myThid)
            CALL MNC_CW_RL_W(pf,'gm_inst',0,0,'Kvz',Kvz,myThid)
          ENDIF
#endif
        ENDIF
#endif /* ALLOW_MNC */

      ENDIF

#endif /* ALLOW_GMREDI */

      RETURN
      END
