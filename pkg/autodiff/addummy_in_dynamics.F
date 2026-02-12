#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#include "AD_CONFIG.h"

CBOP
C     !ROUTINE: ADDUMMY_IN_DYNAMICS
C     !INTERFACE:
      SUBROUTINE ADDUMMY_IN_DYNAMICS( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ADDUMMY_IN_DYNAMICS                           |
C     *==========================================================*
C     Extract adjoint variable from TAF-generated
C     adjoint common blocks, contained in adcommon.h
C     and write fields to file;
C     Make sure common blocks in adcommon.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     | SUBROUTINE ADDUMMY_IN_DYNAMICS                           |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AUTODIFF_PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
c#include "GRID.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "adcommon.h"
# include "AUTODIFF.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
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
c     == local variables ==
C     suff   :: Hold suffix part of a filename
C     msgBuf :: Error message buffer
      CHARACTER*(10) suff
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_AUTODIFF_MONITOR_DIAG
      _RL var3Du(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS dumRS(1)
#endif
CEOP

      IF (
     &  DIFFERENT_MULTIPLE(adjDumpFreq,myTime,deltaTClock)
     & ) THEN

        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

c increment ad dump record number (used only if dumpAdByRec is true)
        dumpAdRecDy=dumpAdRecDy+1
c#ifdef ALLOW_DEBUG
c      IF ( debugMode ) print*,'dumpAdRecDy',dumpAdRecDy
c#endif

        IF ( dumpAdVarExch.EQ.1 ) THEN
#ifdef AUTODIFF_TAMC_COMPATIBILITY
C
#else /* ndfef AUTODIFF_TAMC_COMPATIBILITY */
#   ifdef ALLOW_AUTODIFF_MONITOR_DIAG
         CALL ADEXCH_3D_RL( adrhoinsitu, Nr, myThid )
         CALL ADEXCH_3D_RL( adtotphihyd, Nr, myThid )
#   endif
#endif /* AUTODIFF_TAMC_COMPATIBILITY */
        ENDIF

C--     Set suffix for this set of data files.
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(suff,'(I10.10)') myIter
        ELSE
          CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
        ENDIF

#ifdef ALLOW_AUTODIFF_MONITOR_DIAG
        IF ( dumpAdVarExch.NE.2 ) THEN
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJrhoinsitu.',
     &                          suff, adrhoinsitu, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJrhoinsitu',
     &                      adrhoinsitu, dumpAdRecDy, myIter, myThid )
cph( is set to zero in addiags_phi_hyd
c          CALL WRITE_FLD_XYZ_RL( 'ADJtotphihyd.', suff,
c                                 adtotphihyd, myIter, myThid )
cph)
        ELSE
C       case dumpAdVarExch = 2
          CALL COPY_ADVAR_OUTP( dumRS,adrhoinsitu,var3Du,Nr,12,myThid )
          IF (.NOT.dumpAdByRec) CALL WRITE_FLD_XYZ_RL( 'ADJrhoinsitu.',
     &                          suff, var3Du, myIter, myThid )
          IF (  dumpAdByRec   ) CALL WRITE_REC_XYZ_RL( 'ADJrhoinsitu',
     &                          var3Du, dumpAdRecDy, myIter, myThid )
        ENDIF
#endif

        CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

      ENDIF

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
