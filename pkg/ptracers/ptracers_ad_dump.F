C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_ad_dump.F,v 1.5 2014/09/11 19:18:52 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: ptracers_ad_dump
C     !INTERFACE:
      subroutine ptracers_ad_dump( myTime, myIter, myThid )

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
# include "ptracers_adcommon.h"
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
      INTEGER ip
      CHARACTER*(MAX_LEN_FNAM) suff1,suff2
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF (
     &  DIFFERENT_MULTIPLE(adjDumpFreq,myTime,deltaTClock)
     &   ) THEN

        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

C--     Set suffix for this set of data files.
        WRITE(suff1,'(I10.10)') myIter
C ==>> Resetting run-time parameter writeBinaryPrec in the middle of a run
C ==>>  is very very very nasty !!!
c       writeBinaryPrec = writeStatePrec
C <<==  If you really want to mess-up with this at your own risk,
C <<==  uncomment the line above

        DO ip = 1, PTRACERS_numInUse
         WRITE(suff2,'(A10,A2,A1)')
     &          'ADJptracer',PTRACERS_ioLabel(ip),'.'

         CALL WRITE_FLD_XYZ_RL(
     &        suff2,suff1,
     &        adptracer(1-OLx,1-OLy,1,1,1,ip), 1, myThid )

        ENDDO

        CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

      ENDIF

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
