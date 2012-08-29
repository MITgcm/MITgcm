C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_ad_dump.F,v 1.6 2012/08/29 02:14:13 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"
#include "AD_CONFIG.h"

CBOP
C     !ROUTINE: exf_ad_dump
C     !INTERFACE:
      SUBROUTINE EXF_AD_DUMP( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE exf_ad_dump                           |
C     *==========================================================*
C     Extract adjoint variable from TAMC/TAF-generated
C     adjoint common blocks, contained in adcommon.h
C     and write fields to file;
C     Make sure common blocks in adcommon.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     | SUBROUTINE exf_ad_dump                           |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EXF_PARAM.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#include "GRID.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "adcommon.h"
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
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !LOCAL VARIABLES:
c     == local variables ==
C     suff   :: Hold suffix part of a filename
C     msgBuf :: Error message buffer
      CHARACTER*(MAX_LEN_FNAM) suff
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

c     == end of interface ==
CEOP

      CALL TIMER_START('I/O (WRITE)        [EXF_AD_DUMP]', myThid )

      IF (
     &  DIFFERENT_MULTIPLE(adjDumpFreq,myTime,deltaTClock)
     & ) THEN

C--     Set suffix for this set of data files.
        WRITE(suff,'(I10.10)') myIter
C ==>> Resetting run-time parameter writeBinaryPrec in the middle of a run
C ==>>  is very very very nasty !!!
c       writeBinaryPrec = writeStatePrec
C <<==  If you really want to mess-up with this at your own risk,
C <<==  uncomment the line above

        CALL WRITE_REC_3D_RL(
     &       'ADJhflux.'//suff, writeBinaryPrec, 1,
     &        adhflux, 1, myIter, myThid )
        CALL WRITE_REC_3D_RL(
     &       'ADJsflux.'//suff, writeBinaryPrec, 1,
     &        adsflux, 1, myIter, myThid )
        CALL WRITE_REC_3D_RL(
     &       'ADJustress.'//suff, writeBinaryPrec, 1,
     &        adustress, 1, myIter, myThid )
        CALL WRITE_REC_3D_RL(
     &       'ADJvstress.'//suff, writeBinaryPrec, 1,
     &        advstress, 1, myIter, myThid )

# ifdef ALLOW_ATM_TEMP
        CALL WRITE_REC_3D_RL(
     &       'ADJatemp.'//suff, writeBinaryPrec, 1,
     &        adatemp, 1, myIter, myThid )
        CALL WRITE_REC_3D_RL(
     &       'ADJaqh.'//suff, writeBinaryPrec, 1,
     &        adaqh, 1, myIter, myThid )
        CALL WRITE_REC_3D_RL(
     &       'ADJlwflux.'//suff, writeBinaryPrec, 1,
     &        adlwflux, 1, myIter, myThid )
        CALL WRITE_REC_3D_RL(
     &       'ADJprecip.'//suff, writeBinaryPrec, 1,
     &        adprecip, 1, myIter, myThid )
# endif
      IF ( useAtmWind ) THEN
        CALL WRITE_REC_3D_RL(
     &       'ADJuwind.'//suff, writeBinaryPrec, 1,
     &        aduwind, 1, myIter, myThid )
        CALL WRITE_REC_3D_RL(
     &       'ADJvwind.'//suff, writeBinaryPrec, 1,
     &        advwind, 1, myIter, myThid )
      ENDIF
# ifdef ALLOW_DOWNWARD_RADIATION
        CALL WRITE_REC_3D_RL(
     &       'ADJswdown.'//suff, writeBinaryPrec, 1,
     &        adswdown, 1, myIter, myThid )
        CALL WRITE_REC_3D_RL(
     &       'ADJlwdown.'//suff, writeBinaryPrec, 1,
     &        adlwdown, 1, myIter, myThid )
# endif
# ifdef ATMOSPHERIC_LOADING
cph        CALL WRITE_REC_3D_RL(
cph     &       'ADJapressure.'//suff, writeBinaryPrec, 1,
cph     &        adapressure, 1, myIter, myThid )
# endif
# if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
cph        CALL WRITE_REC_3D_RL(
cph     &       'ADJswflux.'//suff, writeBinaryPrec, 1,
cph     &        adswflux, 1, myIter, myThid )
# endif
# if defined(ALLOW_ATM_TEMP) || defined(EXF_READ_EVAP)
cph        CALL WRITE_REC_3D_RL(
cph     &       'ADJevap.'//suff, writeBinaryPrec, 1,
cph     &        adevap, 1, myIter, myThid )
# endif

      ENDIF

      CALL TIMER_STOP( 'I/O (WRITE)        [EXF_AD_DUMP]', myThid )

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
