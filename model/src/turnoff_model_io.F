#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: TURNOFF_MODEL_IO
C     !INTERFACE:
      SUBROUTINE TURNOFF_MODEL_IO( seqFlag, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE TURNOFF_MODEL_IO
C     | o Turn off some of the model output flags
C     *==========================================================*
C     | Used in adjoint simulation (and called after the first
C     |  forward sweep) to avoid writing output multiple times (if
C     |  recomputations and/or grdchk) with the same iter number.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c#ifdef ALLOW_MNC
c# include "MNC_PARAMS.h"
c#endif
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif
#ifdef ALLOW_OBCS
# include "OBCS_PARAMS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     seqFlag :: flag that indicates where this S/R is called from:
C             :: =0 called at the end of S/R COST_FINAL
C             :: =1 called at initialisation when using DIVA
C     myThid  :: My Thread Id number
      INTEGER seqFlag
      INTEGER myThid

C     !LOCAL VARIABLES:
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      _BARRIER

C--   only master-thread resets shared flags (in common block)
      _BEGIN_MASTER( myThid )

C--   Set output freq. to zero to avoid re-write of
C     averaged fields in reverse checkpointing loops
      monitorFreq  = 0.
      dumpFreq     = 0.
      diagFreq     = 0.
      taveFreq     = 0.
      chkPtFreq    = 0.
      pChkPtFreq   = 0.
      dumpInitAndLast = .FALSE.
      writePickupAtEnd= .FALSE.
      plotLevel    = 0

#ifdef ALLOW_MNC
C--   Not the best solution to fix some MNC output, since it is likely
C     that the same problem exists when we do not use MNC. A better fix
C     is to switch off the specific output freq; leave this commented out.
c     monitor_mnc  = .FALSE.
c     snapshot_mnc = .FALSE.
c     timeave_mnc  = .FALSE.
#endif

#ifdef ALLOW_EXF
C-    should call a S/R EXF_TURNOFF_IO (like SEAICE) to reset this flag
      IF ( useEXF ) exf_monFreq = 0.
#endif

#ifdef ALLOW_OBCS
C-    should call a S/R OBCS_TURNOFF_IO (like SEAICE) to reset this flag
      IF ( useOBCS ) OBCS_monitorFreq = 0.
#endif

#ifdef ALLOW_ECCO
C-    should call a S/R ECCO_TURNOFF_IO (like SEAICE) to reset this flag
      IF ( useECCO ) ecco_output_sterGloH = .FALSE.
#endif

C--   Disable SBO output
      useSBO       = .FALSE.

C--   Disable layers package
      useLayers    = .FALSE.

C--   For now, also disable diagnostics output (might need to change
C     this if we want to use diagnostics to output adjoint-variables)
      useDiagnostics = .FALSE.

      _END_MASTER( myThid )

C--   Package specific S/R to turn-off IO flags (all threads do call S/R)

#ifdef ALLOW_SEAICE
      IF ( useSEAICE ) THEN
        CALL SEAICE_TURNOFF_IO( seqFlag, myThid )
      ENDIF
#endif

#ifdef ALLOW_THSICE
      IF ( useThSIce ) THEN
        CALL THSICE_TURNOFF_IO( seqFlag, myThid )
      ENDIF
#endif

#ifdef ALLOW_PTRACERS
      IF ( usePTRACERS ) THEN
        CALL PTRACERS_TURNOFF_IO( seqFlag, myThid )
      ENDIF
#endif

C--   each thread waits for flags to be reset.
      _BARRIER

      RETURN
      END
