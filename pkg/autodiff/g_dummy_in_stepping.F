#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
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
C     Extract tangent linear variable from TAF-generated
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
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "g_common.h"
#endif

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
CEOP

      IF (
     &  DIFFERENT_MULTIPLE(adjDumpFreq,myTime,deltaTClock)
     &   ) THEN

        CALL TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )
c       write(*,*) 'myIter= ',myIter

C--     Set suffix for this set of data files.
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(suff,'(I10.10)') myIter
        ELSE
          CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
        ENDIF

C--     Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)

        CALL WRITE_FLD_XY_RL ( 'G_Jtaux.',suff, g_fu, myIter, myThid )
        CALL WRITE_FLD_XY_RL ( 'G_Jtauy.',suff, g_fv, myIter, myThid )
        CALL WRITE_FLD_XY_RL ( 'G_Jqnet.',suff, g_qnet, myIter,myThid )
        CALL WRITE_FLD_XY_RL ( 'G_Jempr.',suff, g_empmr,myIter,myThid )
c
        CALL WRITE_FLD_XYZ_RL(
     &       'G_Jtheta.',suff, g_theta, myIter, myThid )
        CALL WRITE_FLD_XYZ_RL(
     &       'G_Jsalt.',suff, g_salt, myIter, myThid )
        CALL WRITE_FLD_XYZ_RL(
     &       'G_Juvel.',suff, g_uvel, myIter, myThid )
        CALL WRITE_FLD_XYZ_RL(
     &       'G_Jvvel.',suff, g_vvel, myIter, myThid )
        CALL WRITE_FLD_XYZ_RL(
     &       'G_Jwvel.',suff, g_wvel, myIter, myThid )
        CALL WRITE_FLD_XY_RL(
     &       'G_Jetan.',suff, g_etan, myIter, myThid )

#ifdef ALLOW_DIFFKR_CONTROL
        CALL WRITE_FLD_XYZ_RL ( 'G_Jdiffkr.',suff, g_diffkr,
     &       myIter, myThid )
#endif
#ifdef ALLOW_KAPGM_CONTROL
        CALL WRITE_FLD_XYZ_RL ( 'G_Jkapgm.',suff, g_kapgm,
     &       myIter, myThid )
#endif
#ifdef ALLOW_KAPREDI_CONTROL
        CALL WRITE_FLD_XYZ_RL ( 'G_Jkapredi.',suff, g_kapredi,
     &       myIter, myThid )
#endif

cph        CALL WRITE_FLD_XY_RL( 'G_J_sst.',suff, g_sst, myIter, myThid )
cph        CALL WRITE_FLD_XY_RL( 'G_J_sss.',suff, g_sss, myIter, myThid )

C--     Reread IO error counter
        endIOErrCount = IO_ERRCOUNT(myThid)

C--     Check for IO errors
        IF ( endIOErrCount .NE. beginIOErrCount ) THEN
         WRITE(msgBuf,'(A)')  'S/R WRITE_STATE'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')  'Error writing out model state'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,I10)') 'Timestep ',myIter
         CALL PRINT_ERROR( msgBuf, myThid )
        ELSE
         WRITE(msgBuf,'(A,I10)')
     &    '// Model state written, timestep', myIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(A)')  ' '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF

        CALL TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

      ENDIF

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_TANGENTLINEAR_RUN */

      RETURN
      END
