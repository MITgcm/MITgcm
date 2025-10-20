#include "EXF_OPTIONS.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: ADEXF_ADJOINT_SNAPSHOTS
C     !INTERFACE:
      SUBROUTINE ADEXF_ADJOINT_SNAPSHOTS(
     I                 iwhen, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ADEXF_ADJOINT_SNAPSHOTS                       |
C     *==========================================================*
C     Adjoint version of EXF Adjoint-variable output subroutine:
C     extract adjoint variable from TAF-generated
C     adjoint common blocks, contained in adcommon.h
C     and write fields to file;
C     Make sure common blocks in adcommon.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     | SUBROUTINE ADEXF_ADJOINT_SNAPSHOTS                       |
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
#ifdef ALLOW_AUTODIFF
#ifdef ALLOW_AUTODIFF_MONITOR
# include "AUTODIFF_PARAMS.h"
# include "AUTODIFF.h"
# include "adcommon.h"
#endif
#endif /* ALLOW_AUTODIFF */

C     !INPUT/OUTPUT PARAMETERS:
C     iwhen  :: indicates where this S/R is called from
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: Thread number for this instance of the routine.
      INTEGER iwhen
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
c     CHARACTER*(10) suff
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RS dumRS(1)
      _RL dumRL(1)
      INTEGER dumpAdRecEx
      LOGICAL doDump
CEOP

C-----------------------------------------------------------------------
C--- do dump?
C-----------------------------------------------------------------------
      doDump = DIFFERENT_MULTIPLE(adjDumpFreq,myTime,deltaTClock)
      IF ( doDump ) THEN
c     determine ad dump record number (used only if dumpAdByRec is true)
       IF (useSeaice.AND.(iWhen.EQ.3)) THEN
        dumpAdRecEx=dumpAdRecMn+1
       ELSE
        dumpAdRecEx=dumpAdRecMn
       ENDIF
c#ifdef ALLOW_DEBUG
c      IF ( debugMode ) print*,'dumpAdRecEx',dumpAdRecEx
c#endif
       CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF

      IF ( iwhen .EQ.1 ) THEN

       CALL DUMP_ADJ_XY_UV(
     &      dumRS, adustress, 'ADJustrs', 'ADJustress.',
     &      dumRS, advstress, 'ADJvstrs', 'ADJvstress.',
     &      34, doDump, dumpAdRecEx, myTime, myIter, myThid )
       CALL DUMP_ADJ_XY( dumRS, adhflux, 'ADJhflux', 'ADJhflux.',
     &      12, doDump, dumpAdRecEx, myTime, myIter, myThid )
       CALL DUMP_ADJ_XY( dumRS, adsflux, 'ADJsflux', 'ADJsflux.',
     &      12, doDump, dumpAdRecEx, myTime, myIter, myThid )

      ELSEIF ( iwhen .EQ.2 ) THEN

# ifdef ALLOW_ATM_TEMP
       CALL DUMP_ADJ_XY( dumRS, adatemp, 'ADJatemp', 'ADJatemp.',
     &      12, doDump, dumpAdRecEx, myTime, myIter, myThid )
       CALL DUMP_ADJ_XY( dumRS, adaqh,   'ADJaqh  ', 'ADJaqh.',
     &      12, doDump, dumpAdRecEx, myTime, myIter, myThid )
       CALL DUMP_ADJ_XY( dumRS, adprecip, 'ADJpreci', 'ADJprecip.',
     &      12, doDump, dumpAdRecEx, myTime, myIter, myThid )
# endif
# ifdef ALLOW_RUNOFF
       CALL DUMP_ADJ_XY( dumRS, adrunoff, 'ADJroff ', 'ADJrunoff.',
     &      12, doDump, dumpAdRecEx, myTime, myIter, myThid )
# endif
       IF ( useAtmWind ) THEN
        CALL DUMP_ADJ_XY( dumRS, aduwind, 'ADJuwind', 'ADJuwind.',
     &       12, doDump, dumpAdRecEx, myTime, myIter, myThid )
        CALL DUMP_ADJ_XY( dumRS, advwind, 'ADJvwind', 'ADJvwind.',
     &       12, doDump, dumpAdRecEx, myTime, myIter, myThid )
       ENDIF
# ifdef ALLOW_DOWNWARD_RADIATION
       CALL DUMP_ADJ_XY( dumRS, adswdown, 'ADJswdn ', 'ADJswdown.',
     &      12, doDump, dumpAdRecEx, myTime, myIter, myThid )
       CALL DUMP_ADJ_XY( dumRS, adlwdown, 'ADJlwdn ', 'ADJlwdown.',
     &      12, doDump, dumpAdRecEx, myTime, myIter, myThid )
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
       CALL DUMP_ADJ_XY( dumRS, adclimsst, 'ADJclsst', 'ADJclimsst.',
     &      12, doDump, dumpAdRecEx, myTime, myIter, myThid )
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
       CALL DUMP_ADJ_XY( dumRS, adclimsss, 'ADJclsst', 'ADJclimsss.',
     &      12, doDump, dumpAdRecEx, myTime, myIter, myThid )
# endif

      ELSEIF ( iwhen .EQ.3 ) THEN

       CALL DUMP_ADJ_XY_UV(
     &      adFu, dumRL, 'ADJtaux ', 'ADJtaux.',
     &      adFv, dumRL, 'ADJtauy ', 'ADJtauy.',
     &      33, doDump, dumpAdRecEx, myTime, myIter, myThid )
       CALL DUMP_ADJ_XY(adQnet, dumRL, 'ADJqnet ', 'ADJqnet.',
     &      11, doDump, dumpAdRecEx, myTime, myIter, myThid )
       CALL DUMP_ADJ_XY(adEmPmR, dumRL, 'ADJempmr', 'ADJempr.',
     &      11, doDump, dumpAdRecEx, myTime, myIter, myThid )
# ifdef SHORTWAVE_HEATING
       CALL DUMP_ADJ_XY(adQsw, dumRL, 'ADJqsw  ', 'ADJqsw.',
     &      11, doDump, dumpAdRecEx, myTime, myIter, myThid )
# endif

      ENDIF

      IF ( doDump ) THEN
#ifdef ALLOW_MNC
        IF (useMNC .AND. autodiff_mnc) THEN

         IF ( iwhen.EQ.1 ) THEN

          CALL MNC_CW_SET_UDIM('adexf', -1, myThid)
          CALL MNC_CW_RL_W_S('D','adexf',0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM('adexf', 0, myThid)
          CALL MNC_CW_I_W_S('I','adexf',0,0,'iter',myIter,myThid)
          CALL MNC_CW_RL_W_S('D','adexf',0,0,'model_time',myTime,
     &         myThid)

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
# ifdef ALLOW_RUNOFF
          CALL MNC_CW_RL_W('D','adexf',0,0,'adrunoff',
     &         adrunoff, myThid)
# endif
          IF ( useAtmWind ) THEN
           CALL MNC_CW_RL_W('D','adexf',0,0,'aduwind',
     &          aduwind, myThid)
           CALL MNC_CW_RL_W('D','adexf',0,0,'advwind',
     &          advwind, myThid)
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

         ENDIF

        ENDIF
#endif /* ALLOW_MNC */

        CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF

#ifdef ALLOW_MONITOR
      CALL ADEXF_MONITOR ( iwhen, myTime, myIter, myThid )
#endif

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_ADJOINT_RUN */

      RETURN
      END
