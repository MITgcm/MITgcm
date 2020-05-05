#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"


CBOP
C     !ROUTINE: DO_STATEVARS_DIAGS
C     !INTERFACE:
      SUBROUTINE DO_STATEVARS_DIAGS( myTime, seqFlag, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DO_STATEVARS_DIAGS
C     | o Controlling routine for state variables diagnostics
C     *==========================================================*
C     | Computing diagnostics of the model state (state-variables)
C     | is done at this level ;
C     | by contrast, other diagnostics (fluxes, tendencies)
C     | remain within the computation sequence.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime  :: Current time of simulation ( s )
C     seqFlag :: flag that indicate where this S/R is called from:
C             :: =0 called from the beginning of forward_step
C             :: =1 called from the middle of forward_step
C             :: =2 called from the end of forward_step
C     myIter  :: Iteration number
C     myThid  :: my Thread Id number
      _RL     myTime
      INTEGER seqFlag
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     !LOCAL VARIABLES:
C     == Local variables ==
C     selectVars :: select which group of dianostics variables to fill-in
C            = 1 :: fill-in diagnostics for tracer   variables only
C            = 2 :: fill-in diagnostics for momentum variables only
C            = 3 :: fill-in diagnostics for momentum & tracer variables
C            = 4 :: fill-in state variable tendency diagnostics the second time
      INTEGER selectVars

C--   Fill-in Diagnostics pkg storage array (for state-variables)
      IF ( usediagnostics ) THEN

C-    select which group of state-var diagnostics to fill-in,
C      depending on: where this S/R is called from (seqFlag)
C                    and stagger/synchronous TimeStep
        selectVars = 0
        IF ( staggerTimeStep ) THEN
          IF ( seqFlag.EQ.0 ) selectVars = 2
          IF ( seqFlag.EQ.1 ) selectVars = 1
        ELSE
          IF ( seqFlag.EQ.0 ) selectVars = 3
        ENDIF
        IF ( seqFlag.EQ.2 ) selectVars = 4
        CALL DIAGNOSTICS_FILL_STATE( selectVars, myIter, myThid )

#ifdef ALLOW_GENERIC_ADVDIFF
        IF ( seqFlag.EQ.0 .AND. useGAD ) THEN
          CALL GAD_DIAGNOSTICS_STATE( myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_GENERIC_ADVDIFF */

#ifdef ALLOW_PTRACERS
        IF ( (selectVars.EQ.1 .OR. selectVars.EQ.3)
     &      .AND. usePTRACERS ) THEN
          CALL PTRACERS_DIAGNOSTICS_STATE( myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_PTRACERS */

#ifdef ALLOW_GMREDI
       IF ( seqFlag.EQ.2 .AND. useGMRedi ) THEN
         CALL GMREDI_DIAGNOSTICS_IMPL( myTime, myIter, myThid )
       ENDIF
#endif /* ALLOW_GMREDI */

#ifdef ALLOW_BBL
       IF ( seqFlag.EQ.0 .AND. useBBL ) THEN
         CALL BBL_DIAGNOSTICS_STATE( myTime, myIter, myThid )
       ENDIF
#endif /* ALLOW_BBL */

#ifdef ALLOW_SEAICE
       IF ( seqFlag.LE.1 .AND. useSEAICE ) THEN
         CALL SEAICE_DIAGNOSTICS_STATE(
     &       seqFlag, myTime, myIter, myThid )
       ENDIF
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_THSICE
       IF ( seqFlag.EQ.0 .AND. useThSIce ) THEN
         CALL THSICE_DIAGNOSTICS_STATE( myTime, myIter, myThid )
       ENDIF
#endif /* ALLOW_THSICE */

#ifdef ALLOW_STREAMICE
       IF ( seqFlag.EQ.2 .AND. useStreamIce ) THEN
         CALL STREAMICE_DIAGNOSTICS_STATE( myTime, myIter, myThid )
       ENDIF
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_LAND
       IF ( seqFlag.EQ.0 .AND. useLand ) THEN
         CALL LAND_DIAGNOSTICS_STATE( myTime, myIter, myThid )
       ENDIF
#endif /* ALLOW_LAND */

#ifdef ALLOW_MYPACKAGE
       IF ( seqFlag.EQ.0 .AND. useMYPACKAGE ) THEN
         CALL MYPACKAGE_DIAGNOSTICS_STATE( myTime, myIter, myThid )
       ENDIF
#endif /* ALLOW_MYPACKAGE */

      ENDIf
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
