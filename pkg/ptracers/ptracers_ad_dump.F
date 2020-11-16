#include "PTRACERS_OPTIONS.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_DIAGNOSTICS
# include "DIAG_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: ptracers_ad_dump
C     !INTERFACE:
      SUBROUTINE ptracers_ad_dump( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ptracers_ad_dump
C     *==========================================================*
C     Extract adjoint variable from TAMC/TAF-generated
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
#include "AUTODIFF_PARAMS.h"
# include "ptracers_adcommon.h"
#endif
#ifdef ALLOW_DIAGNOSTICS
# include "DIAGNOSTICS_SIZE.h"
# include "DIAGNOSTICS.h"
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
C     suff   :: Hold suffix part of a filename
C     msgBuf :: Error message buffer
      INTEGER ip
      CHARACTER*(10) suff
      CHARACTER*(MAX_LEN_FNAM) prefix
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL doExch
      _RS dumRS(1)
      _RL var3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
CEOP

C     Handle exchange options
      doExch = DIFFERENT_MULTIPLE(adjDumpFreq,myTime,deltaTClock)
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

      IF (
     &  DIFFERENT_MULTIPLE(adjDumpFreq,myTime,deltaTClock)
     &   ) THEN

        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

C--     Set suffix for this set of data files.
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(suff,'(I10.10)') myIter
        ELSE
          CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
        ENDIF
C ==>> Resetting run-time parameter writeBinaryPrec in the middle of a run
C ==>>  is very very very nasty !!!
c       writeBinaryPrec = writeStatePrec
C <<==  If you really want to mess-up with this at your own risk,
C <<==  uncomment the line above

        DO ip = 1, PTRACERS_numInUse
         WRITE(prefix,'(A10,A2,A1)')
     &          'ADJptracer', PTRACERS_ioLabel(ip), '.'

         IF (dumpAdVarExch.NE.2) THEN
          CALL WRITE_FLD_XYZ_RL(
     &         prefix, suff,
     &         adptracer(1-OLx,1-OLy,1,1,1,ip), 1, myThid )
         ELSE
          CALL COPY_ADVAR_OUTP(dumRS, adptracer(1-OLx,1-OLy,1,1,1,ip),
     &                        var3D, Nr, 12, myThid)
          CALL WRITE_FLD_XYZ_RL(prefix, suff, var3D, 1, myThid )
         ENDIF

        ENDDO

        CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiag4AdjOutp ) THEN
        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

        DO ip = 1, PTRACERS_numInUse
         WRITE(prefix,'(A6,A2)') 'ADJptr', PTRACERS_ioLabel(ip)
         IF (dumpAdVarExch.NE.2) THEN
          CALL DIAGNOSTICS_FILL( adptracer(1-OLx,1-OLy,1,1,1,ip),
     &           prefix, 0, Nr, 0, 1, 1, myThid )
         ELSE
          CALL COPY_ADVAR_OUTP(dumRS, adptracer(1-OLx,1-OLy,1,1,1,ip),
     &                        var3D, Nr, 12, myThid)
          CALL DIAGNOSTICS_FILL( var3D, prefix, 0, Nr, 0, 1, 1, myThid )
         ENDIF
        ENDDO

        CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF
#endif

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
