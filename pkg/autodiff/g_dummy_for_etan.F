#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_OPENAD
# include "OPENAD_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#include "AD_CONFIG.h"

CBOP
C     !ROUTINE: g_dummy_for_etan
C     !INTERFACE:
      subroutine g_dummy_for_etan( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE g_dummy_for_etan                              |
C     *==========================================================*
C     Extract tangent linear variable from TAMC/TAF-generated
C     tangent linear common blocks, contained in g_common.h
C     and write fields to file;
C     Make sure common blocks in g_common.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     | SUBROUTINE g_dummy_for_etan                              |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#ifdef ALLOW_OPENAD
      use OAD_active
      use OAD_rev
      use OAD_tape
      use OAD_cp
#endif
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# ifdef ALLOW_OPENAD
#  include "DYNVARS.h"
# endif
# include "AUTODIFF_PARAMS.h"
# include "AUTODIFF.h"
# ifndef ALLOW_OPENAD
#  include "g_common.h"
# endif /* ALLOW_OPENAD */
# ifdef ALLOW_MNC
#  include "MNC_PARAMS.h"
# endif
#endif /* ALLOW_AUTODIFF_MONITOR */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: Thread number for this instance of the routine.
      INTEGER myThid
      INTEGER myIter
      _RL     myTime

#ifdef ALLOW_TANGENTLINEAR_RUN
#ifdef ALLOW_AUTODIFF_MONITOR

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !LOCAL VARIABLES:
c     == local variables ==
C     suff   :: Hold suffix part of a filename
C     msgBuf :: Error message buffer
      CHARACTER*(10) suff
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL doDump
      _RS dumRS(1)
      _RL dumRL(1)
CEOP

      doDump = DIFFERENT_MULTIPLE(adjDumpFreq,myTime,deltaTClock)

      IF ( doDump ) THEN
        dumpAdRecEt = dumpAdRecEt + 1
        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF

#ifndef ALLOW_OPENAD
      CALL DUMP_ADJ_XY(dumRS, g_etan, 'TLMetan ', 'G_Jetan.',
     &                 12, doDump, dumpAdRecEt, myTime, myIter, myThid)
#else
      CALL DUMP_ADJ_XY(dumRS, etan%d, 'TLMetan ', 'G_Jetan.',
     &                 12, doDump, dumpAdRecEt, myTime, myIter, myThid)
#endif
      IF ( doDump ) THEN
        CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_TANGENTLINEAR_RUN */

      RETURN
      END
