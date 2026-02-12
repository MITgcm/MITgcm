#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#include "AD_CONFIG.h"

CBOP
C     !ROUTINE: g_dummy_in_dynamics
C     !INTERFACE:
      subroutine g_dummy_in_dynamics( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE g_dummy_in_dynamics                           |
C     *==========================================================*
C     Extract tangent linear variable from TAF-generated
C     tangent linear common blocks, contained in g_common.h
C     and write fields to file;
C     Make sure common blocks in g_common.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     | SUBROUTINE g_dummy_in_dynamics                           |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "g_common.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: Thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_TANGENTLINEAR_RUN
#ifdef ALLOW_AUTODIFF_MONITOR

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
c     == local variables ==
C     suff   :: Hold suffix part of a filename
C     msgBuf :: Error message buffer
      CHARACTER*(10) suff
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF (
     &  DIFFERENT_MULTIPLE(adjDumpFreq,myTime,deltaTClock)
     & ) THEN

        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
c       write(*,*) 'myIter= ',myIter

C--     Set suffix for this set of data files.
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(suff,'(I10.10)') myIter
        ELSE
          CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
        ENDIF

#ifdef ALLOW_AUTODIFF_MONITOR_DIAG
        CALL WRITE_FLD_XYZ_RL(
     &       'G_Jrhoinsitu.',suff, g_rhoinsitu, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(
     &       'G_Jtotphihyd.',suff, g_totphihyd, myIter, myThid)
#endif

        CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

      ENDIF

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_TANGENTLINEAR_RUN */

      RETURN
      END
