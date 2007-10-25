C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_ad_dump.F,v 1.1 2007/10/25 18:41:04 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "EXF_OPTIONS.h"

CBOP
C     !ROUTINE: exf_ad_dump
C     !INTERFACE:
      subroutine exf_ad_dump( mytime, myiter, myThid )

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
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#include "GRID.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "adcommon.h"
#endif

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myIter - iteration counter for this thread
C     myTime - time counter for this thread
C     myThid - Thread number for this instance of the routine.
      integer myThid
      integer myiter
      _RL     mytime

#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
#ifdef ALLOW_AUTODIFF_MONITOR

C     !LOCAL VARIABLES:
c     == local variables ==
C     suff - Hold suffix part of a filename
C     beginIOErrCount - Begin and end IO error counts
C     endIOErrCount
C     msgBuf - Error message buffer
      CHARACTER*(MAX_LEN_FNAM) suff
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf

c     == end of interface ==
CEOP

      call TIMER_START('I/O (WRITE)        [EXF_AD_DUMP]', myThid )

      IF (
     &  DIFFERENT_MULTIPLE(adjDumpFreq,mytime,deltaTClock)
     & ) THEN

       _BARRIER
       _BEGIN_MASTER( myThid )

C--     Set suffix for this set of data files.
        WRITE(suff,'(I10.10)') myIter
        writeBinaryPrec = writeStatePrec

C--     Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)


        CALL MDSWRITEFIELD(
     &       'ADJhflux.'//suff, writeBinaryPrec, globalFiles, 'RL',
     &       1,  adhflux, 1, myIter, myThid )
        CALL MDSWRITEFIELD(
     &       'ADJsflux.'//suff, writeBinaryPrec, globalFiles, 'RL',
     &       1,  adsflux, 1, myIter, myThid )
        CALL MDSWRITEFIELD(
     &       'ADJustress.'//suff, writeBinaryPrec, globalFiles, 'RL',
     &       1,  ustress, 1, myIter, myThid )
        CALL MDSWRITEFIELD(
     &       'ADJvstress.'//suff, writeBinaryPrec, globalFiles, 'RL',
     &       1,  vstress, 1, myIter, myThid )

# ifdef ALLOW_ATM_TEMP
        CALL MDSWRITEFIELD(
     &       'ADJatemp.'//suff, writeBinaryPrec, globalFiles, 'RL',
     &       1,  adatemp, 1, myIter, myThid )
        CALL MDSWRITEFIELD(
     &       'ADJaqh.'//suff, writeBinaryPrec, globalFiles, 'RL',
     &       1,  adaqh, 1, myIter, myThid )
        CALL MDSWRITEFIELD(
     &       'ADJlwflux.'//suff, writeBinaryPrec, globalFiles, 'RL',
     &       1,  adlwflux, 1, myIter, myThid )
        CALL MDSWRITEFIELD(
     &       'ADJprecip.'//suff, writeBinaryPrec, globalFiles, 'RL',
     &       1,  adprecip, 1, myIter, myThid )
# endif
# ifdef ALLOW_ATM_WIND
        CALL MDSWRITEFIELD(
     &       'ADJuwind.'//suff, writeBinaryPrec, globalFiles, 'RL',
     &       1,  aduwind, 1, myIter, myThid )
        CALL MDSWRITEFIELD(
     &       'ADJvwind.'//suff, writeBinaryPrec, globalFiles, 'RL',
     &       1,  advwind, 1, myIter, myThid )
# endif
# ifdef ALLOW_DOWNWARD_RADIATION
        CALL MDSWRITEFIELD(
     &       'ADJswdown.'//suff, writeBinaryPrec, globalFiles, 'RL',
     &       1,  adswdown, 1, myIter, myThid )
        CALL MDSWRITEFIELD(
     &       'ADJlwdown.'//suff, writeBinaryPrec, globalFiles, 'RL',
     &       1,  adlwdown, 1, myIter, myThid )
# endif
# ifdef ATMOSPHERIC_LOADING
        CALL MDSWRITEFIELD(
     &       'ADJapressure.'//suff, writeBinaryPrec, globalFiles, 'RL',
     &       1,  adapressure, 1, myIter, myThid )
# endif
# if defined(ALLOW_ATM_TEMP) || defined(SHORTWAVE_HEATING)
        CALL MDSWRITEFIELD(
     &       'ADJswflux.'//suff, writeBinaryPrec, globalFiles, 'RL',
     &       1,  adswflux, 1, myIter, myThid )
# endif
# if defined(ALLOW_ATM_TEMP) || defined(EXF_READ_EVAP)
        CALL MDSWRITEFIELD(
     &       'ADJevap.'//suff, writeBinaryPrec, globalFiles, 'RL',
     &       1,  adevap, 1, myIter, myThid )
# endif

       _END_MASTER( myThid )
       _BARRIER

      ENDIF

      call TIMER_STOP( 'I/O (WRITE)        [EXF_AD_DUMP]', myThid )

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      end









