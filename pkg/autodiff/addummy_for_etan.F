#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_OPENAD
# include "OPENAD_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#include "AD_CONFIG.h"

CBOP
C     !ROUTINE: ADDUMMY_FOR_ETAN
C     !INTERFACE:
      SUBROUTINE ADDUMMY_FOR_ETAN( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ADDUMMY_FOR_ETAN                              |
C     *==========================================================*
C     Extract adjoint variable for sea level height
C     from TAF-generated
C     adjoint common blocks, contained in adcommon.h
C     and write fields to file;
C     Make sure common blocks in adcommon.h are up-to-date
C     w.r.t. current adjoint code.
C
C     This dummy routine is located inside integr_continuity
C     to get the impact of rstar or surf_dr adjoint
C     variables on adEtaN.
C
C     This does not use the diagnostics package because adEtaN is a
C     "half" time step away from the other adjoint variables, but would
C     have to be written at the same time.
C
C     Created: Tim Smith, tsmith@ices.utexas.edu, Sept 12 2018
C     *==========================================================*
C     | SUBROUTINE ADDUMMY_FOR_ETAN                              |
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
#  include "adcommon.h"
# endif /* ALLOW_OPENAD */
# ifdef ALLOW_MNC
#  include "MNC_PARAMS.h"
# endif
#endif /* ALLOW_AUTODIFF_MONITOR */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime  :: time counter for this thread
C     myIter  :: iteration counter for this thread
C     myThid  :: Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
#ifdef ALLOW_AUTODIFF_MONITOR

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     == local variables ==
C     suff    :: Hold suffix part of a filename
C     msgBuf  :: Error message buffer
c     CHARACTER*(10) suff
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RS dumRS(1)
      LOGICAL doDump
CEOP

      doDump = DIFFERENT_MULTIPLE(adjDumpFreq,myTime,deltaTClock)

      IF ( doDump ) THEN
C increment ad dump record number (used only if dumpAdByRec is true)
C could be just dumpAdRecEt=dumpAdRecMn+1 ????
       dumpAdRecEt = dumpAdRecEt + 1
       CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF

#ifndef ALLOW_OPENAD
      CALL DUMP_ADJ_XY(dumRS, adetan, 'ADJetan ', 'ADJetan.',
     &                 12, doDump, dumpAdRecEt, myTime, myIter,myThid)
#else /* ndef ALLOW_OPENAD */
      CALL DUMP_ADJ_XY(dumRS, etaN%d, 'ADJetan ', 'ADJetan.',
     &                 12, doDump, dumpAdRecEt, myTime, myIter,myThid)
#endif /* ndef ALLOW_OPENAD */

      IF ( doDump ) THEN

C-- NOW NMC output
#ifndef ALLOW_OPENAD
#ifdef ALLOW_MNC
        IF ( useMNC .AND. autodiff_mnc ) THEN
         CALL MNC_CW_SET_UDIM('adstate', -1, myThid)
         CALL MNC_CW_RL_W_S('D','adstate',0,0,'T',myTime,myThid)
         CALL MNC_CW_SET_UDIM('adstate', 0, myThid)
         CALL MNC_CW_I_W_S('I','adstate',0,0,'iter',myIter,myThid)
         CALL MNC_CW_RL_W_S('D','adstate',0,0,'model_time',myTime,
     &        myThid)
         CALL MNC_CW_RL_W('D','adstate',0,0,'adEta', adEtaN, myThid)
C     endif mnc
        ENDIF
#endif /* ALLOW_MNC */
#endif /* ALLOW_OPENAD */

        CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

      ENDIF

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
