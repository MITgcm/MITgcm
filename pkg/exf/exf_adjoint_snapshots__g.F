#include "EXF_OPTIONS.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: G_EXF_ADJOINT_SNAPSHOTS
C     !INTERFACE:
      SUBROUTINE G_EXF_ADJOINT_SNAPSHOTS(
     I                 iwhen, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE G_EXF_ADJOINT_SNAPSHOTS                       |
C     *==========================================================*
C     Tangent-Linear version of EXF Adjoint-variable output S/R:
C     extract tangent linear variable from TAF-generated
C     tangent linear common blocks, contained in g_common.h
C     and write fields to file;
C     Make sure common blocks in g_common.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     | SUBROUTINE G_EXF_ADJOINT_SNAPSHOTS                       |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EXF_PARAM.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "g_common.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     iwhen  :: indicates where this S/R is called from
C     myIter :: Iteration counter for this thread
C     myTime :: Time counter for this thread
C     myThid :: Thread number for this instance of the routine.
      INTEGER iwhen
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_TANGENTLINEAR_RUN
#ifdef ALLOW_AUTODIFF_MONITOR

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     suff   :: Hold suffix part of a filename
C     msgBuf :: Error message buffer
      CHARACTER*(10) suff
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF (useEXF) THEN
       IF (
     &      DIFFERENT_MULTIPLE(adjDumpFreq,myTime,deltaTClock)
     &    ) THEN

        CALL TIMER_START('I/O (WRITE)        [TLM LOOP]', myThid )

C--     Set suffix for this set of data files.
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(suff,'(I10.10)') myIter
        ELSE
          CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
        ENDIF

        IF ( iwhen .EQ.1 ) THEN

        CALL WRITE_FLD_XY_RL ( 'G_Justress.',
     &       suff, g_ustress, myIter, myThid )
        CALL WRITE_FLD_XY_RL ( 'G_Jvstress.',
     &       suff, g_vstress, myIter, myThid )
        CALL WRITE_FLD_XY_RL ( 'G_Jhflux.',
     &       suff, g_hflux, myIter, myThid )
        CALL WRITE_FLD_XY_RL ( 'G_Jsflux.',
     &       suff, g_sflux, myIter, myThid )

        ELSEIF ( iwhen .EQ.2 ) THEN

# ifdef ALLOW_ATM_TEMP
        CALL WRITE_FLD_XY_RL ( 'G_Jatemp.',
     &       suff, g_atemp, myIter, myThid )
        CALL WRITE_FLD_XY_RL ( 'G_Jaqh.',
     &       suff, g_aqh, myIter, myThid )
        CALL WRITE_FLD_XY_RL ( 'G_Jprecip.',
     &       suff, g_precip, myIter, myThid )
# endif
        IF ( useAtmWind ) THEN
          CALL WRITE_FLD_XY_RL ( 'G_Juwind.',
     &         suff, g_uwind, myIter, myThid )
          CALL WRITE_FLD_XY_RL ( 'G_Jvwind.',
     &         suff, g_vwind, myIter, myThid )
        ENDIF
# ifdef ALLOW_DOWNWARD_RADIATION
        CALL WRITE_FLD_XY_RL ( 'G_Jswdown.',
     &       suff, g_swdown, myIter, myThid )
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
        CALL WRITE_FLD_XY_RL ( 'G_Jclimsst.',
     &       suff, g_climsst, myIter, myThid )
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
        CALL WRITE_FLD_XY_RL ( 'G_Jclimsss.',
     &       suff, g_climsss, myIter, myThid )
# endif

        ENDIF

        CALL TIMER_STOP( 'I/O (WRITE)        [TLM LOOP]', myThid )

       ENDIF
      ENDIF

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_TANGENTLINEAR_RUN */

      RETURN
      END
