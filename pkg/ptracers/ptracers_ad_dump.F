#include "PTRACERS_OPTIONS.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: ptracers_ad_dump
C     !INTERFACE:
      SUBROUTINE ptracers_ad_dump( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ptracers_ad_dump
C     *==========================================================*
C     Extract adjoint variable from TAF-generated
C     adjoint common blocks, contained in adcommon.h
C     and write fields to file;
C     Make sure common blocks in adcommon.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     | SUBROUTINE ptracers_ad_dump
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "AUTODIFF.h"
# include "AUTODIFF_PARAMS.h"
# include "ptracers_adcommon.h"
#endif
#ifdef ALLOW_DIAGNOSTICS
# include "DIAGNOSTICS_P2SHARE.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: Thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
#ifdef ALLOW_AUTODIFF_MONITOR

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     msgBuf :: Error message buffer
      INTEGER ip
      CHARACTER*(8)  diagName
      CHARACTER*(MAX_LEN_FNAM) dumpName
      LOGICAL doExch, doDump
      _RS dumRS(1)
CEOP

C     Handle exchange options
      doDump = DIFFERENT_MULTIPLE(adjDumpFreq,myTime,deltaTClock)
      doExch = doDump
#ifdef ALLOW_DIAGNOSTICS
      doExch = doExch.OR.useDiag4AdjOutp
#endif
      IF (doExch) THEN
       IF (dumpAdVarExch.EQ.1) THEN
        DO ip = 1, PTRACERS_numInUse
#ifdef AUTODIFF_TAMC_COMPATIBILITY
         CALL ADEXCH_XYZ_RL(myThid, adptracer(1-OLx,1-OLy,1,1,1,ip))
#else
         CALL ADEXCH_3D_RL(adptracer(1-OLx,1-OLy,1,1,1,ip), Nr, myThid)
#endif
        ENDDO
       ENDIF
      ENDIF

      IF (doDump) THEN
        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF

      DO ip = 1, PTRACERS_numInUse
        WRITE(diagName,'(A6,A2)') 'ADJptr', PTRACERS_ioLabel(ip)
        WRITE(dumpName,'(A10,A2,A1)')
     &        'ADJptracer', PTRACERS_ioLabel(ip), '.'

        CALL DUMP_ADJ_XYZ( dumRS, adptracer(1-OLx,1-OLy,1,1,1,ip),
     &                     diagName, dumpName, 12, doDump,
     &                     dumpAdRecMn, myTime, myIter, myThid )
      ENDDO

      IF (doDump) THEN
        CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
