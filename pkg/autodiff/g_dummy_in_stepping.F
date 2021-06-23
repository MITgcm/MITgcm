#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_OPENAD
# include "OPENAD_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_DIAGNOSTICS
# include "DIAG_OPTIONS.h"
#endif
#ifdef ALLOW_GGL90
# include "GGL90_OPTIONS.h"
#endif
#ifdef ALLOW_SHELFICE
# include "SHELFICE_OPTIONS.h"
#endif
#include "AD_CONFIG.h"

CBOP
C     !ROUTINE: g_dummy_in_stepping
C     !INTERFACE:
      subroutine g_dummy_in_stepping( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE g_dummy_in_stepping                           |
C     *==========================================================*
C     Extract tangent linear variable from TAMC/TAF-generated
C     tangent linear common blocks, contained in g_common.h
C     and write fields to file;
C     Make sure common blocks in g_common.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     | SUBROUTINE g_dummy_in_stepping                           |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#ifdef ALLOW_OPENAD
      use OAD_active
      use OAD_rev
      use OAD_tape
      use OAD_cp
#endif
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# ifdef ALLOW_OPENAD
C- Note: Since OpenAD uses modules, the ordering of included headers matters
#  include "DYNVARS.h"
#  include "FFIELDS.h"
#  ifdef ALLOW_GGL90
#   include "GGL90.h"
#  endif
# endif /* ALLOW_OPENAD */
# include "AUTODIFF_PARAMS.h"
# include "AUTODIFF.h"
# ifndef ALLOW_OPENAD
#  include "g_common.h"
# endif /* ALLOW_OPENAD */
# ifdef ALLOW_MNC
#  include "MNC_PARAMS.h"
# endif
# ifdef ALLOW_DIAGNOSTICS
#  include "DIAGNOSTICS_SIZE.h"
#  include "DIAGNOSTICS.h"
# endif
#endif /* ALLOW_AUTODIFF_MONITOR */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: Thread number for this instance of the routine.
      INTEGER myThid
      INTEGER myIter
      _RL     myTime

#ifdef ALLOW_TANGENTLINEAR_RUN
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
      CHARACTER*(10) suff
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RS dumRS(1)
      _RL dumRL(1)
      LOGICAL doDump
CEOP

      doDump = DIFFERENT_MULTIPLE(adjDumpFreq, myTime, deltaTClock)

      IF ( doDump ) THEN
        dumpAdRecMn = dumpAdRecMn + 1
        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF

#ifndef ALLOW_OPENAD
C========================
C--  TAF AD-variables:
C========================
C-----------------------------------------------------------------------
C--- State variables
C-----------------------------------------------------------------------
      CALL DUMP_ADJ_XYZ(dumRS, g_theta, 'TLMtheta', 'G_Jtheta.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter, myThid)
      CALL DUMP_ADJ_XYZ(dumRS, g_salt, 'TLMsalt ', 'G_Jsalt.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter, myThid)
      CALL DUMP_ADJ_XYZ(dumRS, g_wvel, 'TLMwvel ', 'G_Jwvel.',
     &                  12, doDump, dumpAdRecMn, myTime, myIter, myThid)
      CALL DUMP_ADJ_XYZ_UV(
     &      dumRS, g_uvel, 'TLMuvel ', 'G_Juvel.',
     &      dumRS, g_vvel, 'TLMvvel ', 'G_Jvvel.',
     &      34, doDump, dumpAdRecMn, myTime, myIter, myThid)

C-----------------------------------------------------------------------
C--- Surface Forcing
C-----------------------------------------------------------------------
CCC TODO: add the diagnostics in the right place
CCC      IF ( .NOT. useSEAICE .AND. .NOT. useEXF ) THEN
CCC       CALL DUMP_ADJ_XY_UV(
CCC     &      g_Fu, dumRL, 'TLMtaux ', 'G_Jtaux.',
CCC     &      g_Fv, dumRL, 'TLMtauy ', 'G_Jtauy.',
CCC     &      33, doDump, dumpAdRecMn, myTime, myIter,myThid)
CCC        CALL DUMP_ADJ_XY(g_Qnet, dumRL, 'TLMqnet ', 'G_Jqnet.',
CCC     &                   11, doDump, dumpAdRecMn, myTime, myIter,myThid)
CCC        CALL DUMP_ADJ_XY(g_EmPmR, dumRL, 'TLMempmr', 'G_Jempr.',
CCC     &                   11, doDump, dumpAdRecMn, myTime, myIter,myThid)

CCC# ifdef SHORTWAVE_HEATING
CCC        CALL DUMP_ADJ_XY(g_Qsw, dumRL, 'TLMqsw  ', 'G_Jqsw.',
CCC     &                   11, doDump, dumpAdRecMn, myTime, myIter,myThid)
CCC# endif
CCC      ENDIF
CCC# ifdef ALLOW_SST0_CONTROL
CCC      CALL DUMP_ADJ_XY(g_SST, dumRL, 'TLMsst  ', 'G_Jsst.',
CCC     &                 11, doDump, dumpAdRecMn, myTime, myIter,myThid)
CCC# endif
CCC# ifdef ALLOW_SSS0_CONTROL
CCC      CALL DUMP_ADJ_XY(g_SSS, dumRL, 'TLMsss  ', 'G_Jsss.',
CCC     &                 11, doDump, dumpAdRecMn, myTime, myIter,myThid)
CCC# endif

C-----------------------------------------------------------------------
C---Various packages and parameters
C-----------------------------------------------------------------------
CCC TODO: add the diagnostics in the right place
CCC# ifdef ALLOW_BOTTOMDRAG_CONTROL
CCC      CALL DUMP_ADJ_XY(dumRS, g_BottomDragFld,
CCC     &                 'TLMbtdrg','G_Jbottomdrag.',
CCC     &                 12, doDump, dumpAdRecMn, myTime, myIter,myThid)
CCC# endif
CCC# ifdef ALLOW_GGL90
CCC      CALL DUMP_ADJ_XYZ(dumRS, g_GGL90TKE, 'TLMtke90', 'G_Jggl90tke.',
CCC     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)
CCC# endif
CCC# ifdef ALLOW_DIFFKR_CONTROL
CCC      CALL DUMP_ADJ_XYZ(dumRS, g_DiffKr, 'TLMdifkr', 'G_Jdiffkr.',
CCC     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)
CCC# endif
CCC# ifdef ALLOW_KAPGM_CONTROL
CCC      CALL DUMP_ADJ_XYZ(dumRS, g_KapGM, 'TLMkapgm', 'G_Jkapgm.',
CCC     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)
CCC# endif
CCC# ifdef ALLOW_KAPREDI_CONTROL
CCC      CALL DUMP_ADJ_XYZ(dumRS, g_KapRedi, 'TLMkapre', 'G_Jkapredi.',
CCC     &                  12, doDump, dumpAdRecMn, myTime, myIter,myThid)
CCC# endif
CCC
CCC# ifdef ALLOW_EDDYPSI_CONTROL
CCC      CALL DUMP_ADJ_XYZ_UV(
CCC     &     g_EddyPsiX,dumRL,'TLMepsix','G_Jeddipsix.',
CCC     &     g_EddyPsiY,dumRL,'TLMepsiy','G_Jeddipsiy.',
CCC     &     33, doDump, dumpAdRecMn, myTime, myIter,myThid)
CCC# endif
# ifdef ALLOW_SHELFICE
      CALL DUMP_ADJ_XY(g_shelficeFreshwaterFlux, dumRL,
     &                 'TLMshfwf','G_Jshifwf.',
     &                 11, doDump, dumpAdRecMn, myTime, myIter,myThid)
      CALL DUMP_ADJ_XY(g_shelficeHeatFlux, dumRL,
     &                 'TLMshhtf','G_Jshihtf.',
     &                 11, doDump, dumpAdRecMn, myTime, myIter,myThid)
      CALL DUMP_ADJ_XY(dumRS, g_shiTransCoeffT,
     &                 'TLMshict','G_Jshict.',
     &                 12, doDump, dumpAdRecMn, myTime, myIter,myThid)
      CALL DUMP_ADJ_XY(dumRS, g_shiTransCoeffS,
     &                 'TLMshics','G_Jshict.',
     &                 12, doDump, dumpAdRecMn, myTime, myIter,myThid)
#  if (defined SHI_ALLOW_GAMMAFRICT)
      CALL DUMP_ADJ_XY(dumRS, g_shiCDragFld,
     &                 'TLMshicd','G_Jshicd.',
     &                 12, doDump, dumpAdRecMn, myTime, myIter,myThid)
#  endif
# endif

CCC TODO: add diagnostics and stuff
CCC#ifdef ALLOW_AUTODIFF_MONITOR_PHIHYD
CCC      CALL WRITE_FLD_XYZ_RL(
CCC     &     'G_Jphihyd.',suff, g_totphihyd, myIter, myThid )
CCC#endif
#endif /* not ALLOW_OPENAD */


      IF (doDump) THEN
        CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )
      ENDIF

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_TANGENTLINEAR_RUN */

      RETURN
      END
