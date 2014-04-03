C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_adjoint_snapshots_ad.F,v 1.11 2014/04/03 10:00:58 atn Exp $
C $Name:  $

#include "EXF_OPTIONS.h"
#include "AD_CONFIG.h"

CBOP
C     !ROUTINE: adexf_adjoint_snapshots
C     !INTERFACE:
      subroutine adexf_adjoint_snapshots(
     &     iwhen, myTime, myIter, myThid )

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
#include "EXF_PARAM.h"
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif
#include "GRID.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "AUTODIFF_PARAMS.h"
# include "AUTODIFF.h"
# include "adcommon.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: Thread number for this instance of the routine.
      integer iwhen
      _RL     myTime
      integer myIter
      integer myThid

#if (defined (ALLOW_ADJOINT_RUN) || defined (ALLOW_ADMTLM))
#ifdef ALLOW_AUTODIFF_MONITOR

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
c     == local variables ==
C     suff   :: Hold suffix part of a filename
C     msgBuf :: Error message buffer
      CHARACTER*(MAX_LEN_FNAM) suff
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER dumpAdRecEx
CEOP

      CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

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

c determine ad dump record number (used only if dumpAdByRec is true)
        IF (useSeaice.AND.(iWhen.EQ.3)) THEN
          dumpAdRecEx=dumpAdRecMn+1
        ELSE
          dumpAdRecEx=dumpAdRecMn
        ENDIF
c#ifdef ALLOW_DEBUG
c      IF ( debugMode ) print*,'dumpAdRecEx',dumpAdRecEx
c#endif

        IF (.NOT.dumpAdByRec) THEN

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
      IF ( useAtmWind ) THEN
        CALL WRITE_FLD_XY_RL ( 'ADJuwind.',
     &       suff, aduwind, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJvwind.',
     &       suff, advwind, myIter, myThid)
      ENDIF
# ifdef ALLOW_DOWNWARD_RADIATION
        CALL WRITE_FLD_XY_RL ( 'ADJswdown.',
     &       suff, adswdown, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJlwdown.',
     &       suff, adlwdown, myIter, myThid)
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
        CALL WRITE_FLD_XY_RL ( 'ADJclimsst.',
     &       suff, adclimsst, myIter, myThid)
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
        CALL WRITE_FLD_XY_RL ( 'ADJclimsss.',
     &       suff, adclimsss, myIter, myThid)
# endif

        ELSEIF ( iwhen .EQ.3 ) THEN

        CALL WRITE_FLD_XY_RS ( 'ADJtaux.',suff, adfu, myIter, myThid)
        CALL WRITE_FLD_XY_RS ( 'ADJtauy.',suff, adfv, myIter, myThid)
        CALL WRITE_FLD_XY_RS ( 'ADJqnet.',suff, adqnet, myIter, myThid)
        CALL WRITE_FLD_XY_RS ( 'ADJempr.',suff, adempmr, myIter, myThid)
#ifdef SHORTWAVE_HEATING
        CALL WRITE_FLD_XY_RS ( 'ADJqsw.',suff, adqsw, myIter, myThid)
#endif

        ENDIF

        ELSEIF ( dumpAdRecEx .GT. 0 ) THEN

        IF ( iwhen .EQ.1 ) THEN

        CALL WRITE_REC_XY_RL ( 'ADJustress',
     &       adustress, dumpAdRecEx, myIter, myThid)
        CALL WRITE_REC_XY_RL ( 'ADJvstress',
     &       advstress, dumpAdRecEx, myIter, myThid)
        CALL WRITE_REC_XY_RL ( 'ADJhflux',
     &       adhflux, dumpAdRecEx, myIter, myThid)
        CALL WRITE_REC_XY_RL ( 'ADJsflux',
     &       adsflux, dumpAdRecEx, myIter, myThid)

        ELSEIF ( iwhen .EQ.2 ) THEN

# ifdef ALLOW_ATM_TEMP
        CALL WRITE_REC_XY_RL ( 'ADJatemp',
     &       adatemp, dumpAdRecEx, myIter, myThid)
        CALL WRITE_REC_XY_RL ( 'ADJaqh',
     &       adaqh, dumpAdRecEx, myIter, myThid)
        CALL WRITE_REC_XY_RL ( 'ADJprecip',
     &       adprecip, dumpAdRecEx, myIter, myThid)
# endif
      IF ( useAtmWind ) THEN
        CALL WRITE_REC_XY_RL ( 'ADJuwind',
     &       aduwind, dumpAdRecEx, myIter, myThid)
        CALL WRITE_REC_XY_RL ( 'ADJvwind',
     &       advwind, dumpAdRecEx, myIter, myThid)
      ENDIF
# ifdef ALLOW_DOWNWARD_RADIATION
        CALL WRITE_REC_XY_RL ( 'ADJswdown',
     &       adswdown, dumpAdRecEx, myIter, myThid)
        CALL WRITE_REC_XY_RL ( 'ADJlwdown',
     &       adlwdown, dumpAdRecEx, myIter, myThid)
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
        CALL WRITE_REC_XY_RL ( 'ADJclimsst',
     &       adclimsst, dumpAdRecEx, myIter, myThid)
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
        CALL WRITE_REC_XY_RL ( 'ADJclimsss',
     &       adclimsss, dumpAdRecEx, myIter, myThid)
# endif

        ELSEIF ( iwhen .EQ.3 ) THEN

        CALL WRITE_REC_XY_RS ( 'ADJtaux',
     &       adfu, dumpAdRecEx, myIter, myThid)
        CALL WRITE_REC_XY_RS ( 'ADJtauy',
     &       adfv, dumpAdRecEx, myIter, myThid)
        CALL WRITE_REC_XY_RS ( 'ADJqnet',
     &       adqnet, dumpAdRecEx, myIter, myThid)
        CALL WRITE_REC_XY_RS ( 'ADJempr',
     &       adempmr, dumpAdRecEx, myIter, myThid)
#ifdef SHORTWAVE_HEATING
        CALL WRITE_REC_XY_RS ( 'ADJqsw',
     &       adqsw, dumpAdRecEx, myIter, myThid)
#endif

        ENDIF

        ENDIF

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
      IF ( useAtmWind ) THEN
          CALL MNC_CW_RL_W('D','adexf',0,0,'aduwind',
     &         aduwind, myThid)
          CALL MNC_CW_RL_W('D','adexf',0,0,'advwind',
     &         advwind, myThid)
      ENDIF
# ifdef ALLOW_DOWNWARD_RADIATION
          CALL MNC_CW_RL_W('D','adexf',0,0,'adswdown',
     &         adswdown, myThid)
          CALL MNC_CW_RL_W('D','adexf',0,0,'adlwdown',
     &         adlwdown, myThid)
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

#ifdef ALLOW_MONITOR
      CALL ADEXF_MONITOR ( iwhen, myTime, myIter, myThid )
#endif

      CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
