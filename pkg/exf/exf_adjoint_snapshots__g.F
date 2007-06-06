
#include "EXF_OPTIONS.h"

CBOP
C     !ROUTINE: g_exf_adjoint_snapshots
C     !INTERFACE:
      subroutine g_exf_adjoint_snapshots( 
     &     iwhen, mytime, myiter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE g_exf_adjoint_snapshots                       |
C     *==========================================================*
C     Extract tangent linear variable from TAMC/TAF-generated
C     tangent linear common blocks, contained in g_common.h
C     and write fields to file;
C     Make sure common blocks in g_common.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     | SUBROUTINE g_exf_adjoint_snapshots                       |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "g_common.h"
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

#ifdef ALLOW_TANGENTLINEAR_RUN
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

      call TIMER_START('I/O (WRITE)        [TLM LOOP]', myThid )

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

        CALL WRITE_FLD_XY_RL ( 'G_Justress.',
     &       suff, g_ustress, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'G_Jvstress.',
     &       suff, g_vstress, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'G_Jhflux.',
     &       suff, g_hflux, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'G_Jsflux.',
     &       suff, g_sflux, myIter, myThid)

        ELSEIF ( iwhen .EQ.2 ) THEN

# ifdef ALLOW_ATM_TEMP
        CALL WRITE_FLD_XY_RL ( 'G_Jatemp.',
     &       suff, g_atemp, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'G_Jaqh.',
     &       suff, g_aqh, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'G_Jprecip.',
     &       suff, g_precip, myIter, myThid)
# endif
# ifdef ALLOW_ATM_WIND
        CALL WRITE_FLD_XY_RL ( 'G_Juwind.',
     &       suff, g_uwind, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'G_Jvwind.',
     &       suff, g_vwind, myIter, myThid)
# endif
# ifdef ALLOW_DOWNWARD_RADIATION
        CALL WRITE_FLD_XY_RL ( 'G_Jswdown.',
     &       suff, g_swdown, myIter, myThid)
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
        CALL WRITE_FLD_XY_RL ( 'G_Jclimsst.',
     &       suff, g_climsst, myIter, myThid)
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
        CALL WRITE_FLD_XY_RL ( 'G_Jclimsss.',
     &       suff, g_climsss, myIter, myThid)
# endif

        ENDIF

       _END_MASTER( myThid )
       _BARRIER


      ENDIF

      ENDIF

      call TIMER_STOP( 'I/O (WRITE)        [TLM LOOP]', myThid )

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_TANGENTLINEAR_RUN */

      end









