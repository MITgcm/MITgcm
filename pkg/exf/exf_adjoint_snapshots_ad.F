C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_adjoint_snapshots_ad.F,v 1.1 2007/06/05 21:51:22 heimbach Exp $

#include "PACKAGES_CONFIG.h"
#include "AUTODIFF_OPTIONS.h"
#include "EXF_OPTIONS.h"

CBOP
C     !ROUTINE: adexf_adjoint_snapshots
C     !INTERFACE:
      subroutine adexf_adjoint_snapshots( 
     &     iwhen, mytime, myiter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE adexf_adjoint_snapshots                       |
C     *==========================================================*
C     Extract adjoint variable from TAMC/TAF-generated
C     adjoint common blocks, contained in adcommon.h
C     and write fields to file;
C     Make sure common blocks in adcommon.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     | SUBROUTINE adexf_adjoint_snapshots                       |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
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
      integer iwhen
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

      call TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

      IF (useEXF) THEN

      IF (
     &  DIFFERENT_MULTIPLE(adjDumpFreq,mytime,deltaTClock)
     & ) THEN

       _BEGIN_MASTER( myThid )

C--     Set suffix for this set of data files.
        WRITE(suff,'(I10.10)') myIter
        writeBinaryPrec = writeStatePrec

C--     Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)

        IF ( iwhen .EQ.1 ) THEN

        CALL WRITE_FLD_XY_RL ( 'ADJustress.',
     &       suff, adustress, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJvstress.',
     &       suff, advstress, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJhflux.',
     &       suff, adhflux, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJsflux.',
     &       suff, adsflux, myIter, myThid)

        ELSEIF ( iwhen .EQ.2 ) THEN

# ifdef ALLOW_ATM_TEMP
        CALL WRITE_FLD_XY_RL ( 'ADJatemp.',
     &       suff, adatemp, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJaqh.',
     &       suff, adaqh, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJprecip.',
     &       suff, adprecip, myIter, myThid)
# endif
# ifdef ALLOW_ATM_WIND
        CALL WRITE_FLD_XY_RL ( 'ADJuwind.',
     &       suff, aduwind, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJvwind.',
     &       suff, advwind, myIter, myThid)
# endif
# ifdef ALLOW_DOWNWARD_RADIATION
        CALL WRITE_FLD_XY_RL ( 'ADJswdown.',
     &       suff, adswdown, myIter, myThid)
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
        CALL WRITE_FLD_XY_RL ( 'ADJclimsst.',
     &       suff, adclimsst, myIter, myThid)
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
        CALL WRITE_FLD_XY_RL ( 'ADJclimsss.',
     &       suff, adclimsss, myIter, myThid)
# endif

        ENDIF

       _END_MASTER( myThid )
       _BARRIER

#ifdef ALLOW_MNC
        IF (useMNC .AND. autodiff_mnc) THEN

          IF ( iwhen.EQ.1 ) THEN
c
          CALL MNC_CW_SET_UDIM('adexf', -1, myThid)
          CALL MNC_CW_RL_W_S('D','adexf',0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM('adexf', 0, myThid)
          CALL MNC_CW_I_W_S('I','adexf',0,0,'iter',myIter,myThid)
          CALL MNC_CW_RL_W_S('D','adexf',0,0,'model_time',myTime,
     &         myThid)
c
          CALL MNC_CW_RL_W('D','adexf',0,0,'adustress', 
     &         adustress, myThid)
          CALL MNC_CW_RL_W('D','adexf',0,0,'advstress',
     &         advstress, myThid)
          CALL MNC_CW_RL_W('D','adexf',0,0,'adhflux',
     &         adhflux, myThid)
          CALL MNC_CW_RL_W('D','adexf',0,0,'adsflux',
     &         adsflux, myThid)
          ELSEIF ( iwhen.EQ.2 ) THEN
# ifdef ALLOW_ATM_TEMP
          CALL MNC_CW_RL_W('D','adexf',0,0,'adatemp',
     &         adatemp, myThid)
          CALL MNC_CW_RL_W('D','adexf',0,0,'adaqh',
     &         adaqh, myThid)
          CALL MNC_CW_RL_W('D','adexf',0,0,'adprecip',
     &         adprecip, myThid)
# endif
# ifdef ALLOW_ATM_WIND
          CALL MNC_CW_RL_W('D','adexf',0,0,'aduwind',
     &         aduwind, myThid)
          CALL MNC_CW_RL_W('D','adexf',0,0,'advwind',
     &         advwind, myThid)
# endif
# ifdef ALLOW_DOWNWARD_RADIATION
          CALL MNC_CW_RL_W('D','adexf',0,0,'adswdown',
     &         adswdown, myThid)
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
          CALL MNC_CW_RL_W('D','adexf',0,0,'adclimsst',
     &         adclimsst, myThid)
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
          CALL MNC_CW_RL_W('D','adexf',0,0,'adclimsss',
     &         adclimsss, myThid)
# endif
c
          ENDIF

        ENDIF
#endif /* ALLOW_MNC */

      ENDIF

      ENDIF

      call TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      end









