
#include "AUTODIFF_OPTIONS.h"

CBOP
C     !ROUTINE: g_dummy_in_stepping
C     !INTERFACE:
      subroutine g_dummy_in_stepping( mytime, myiter, myThid )

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
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_MONITOR
# include "g_common.h"
#endif

      LOGICAL  DIFF_BASE_MULTIPLE
      EXTERNAL DIFF_BASE_MULTIPLE
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

      call TIMER_START('I/O (WRITE)        [ADJOINT LOOP]', myThid )

      IF (
     &  DIFF_BASE_MULTIPLE(baseTime,adjDumpFreq,mytime,deltaTClock)
     & ) THEN

         write(*,*) 'myIter= ',myiter

       _BARRIER
       _BEGIN_MASTER( myThid )

C--     Set suffix for this set of data files.
        WRITE(suff,'(I10.10)') myIter
        writeBinaryPrec = writeStatePrec

C--     Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)

        CALL WRITE_FLD_XY_RL ( 'G_Jtaux.',suff, g_fu, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'G_Jtauy.',suff, g_fv, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'G_Jqnet.',suff, g_qnet, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'G_Jempr.',suff, g_empmr, myIter, myThid)
c
        CALL WRITE_FLD_XYZ_RL( 
     &       'G_Jtheta.',suff, g_theta, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL( 
     &       'G_Jsalt.',suff, g_salt, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL( 
     &       'G_Juvel.',suff, g_uvel, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL( 
     &       'G_Jvvel.',suff, g_vvel, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(
     &       'G_Jwvel.',suff, g_vvel, myIter, myThid)
        CALL WRITE_FLD_XY_RL(
     &       'G_Jetan.',suff, g_vvel, myIter, myThid)

#ifdef ALLOW_DIFFKR_CONTROL
        CALL WRITE_FLD_XYZ_RL ( 'G_Jdiffkr.',suff, g_diffkr, 
     &       myIter, myThid)
#endif
#ifdef ALLOW_KAPGM_CONTROL
        CALL WRITE_FLD_XYZ_RL ( 'G_Jkagm.',suff, g_kapgm, 
     &       myIter, myThid)
#endif

cph        CALL WRITE_FLD_XY_RL( 'G_J_sst.',suff, g_sst, myIter, myThid)
cph        CALL WRITE_FLD_XY_RL( 'G_J_sss.',suff, g_sss, myIter, myThid)

C--     Reread IO error counter
        endIOErrCount = IO_ERRCOUNT(myThid)

C--     Check for IO errors
        IF ( endIOErrCount .NE. beginIOErrCount ) THEN
         WRITE(msgBuf,'(A)')  'S/R WRITE_STATE'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A)')  'Error writing out model state'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A,I10)') 'Timestep ',myIter
         CALL PRINT_ERROR( msgBuf, 1 )
        ELSE
         WRITE(msgBuf,'(A,I10)')  
     &    '// Model state written, timestep', myIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT, 1 )
         WRITE(msgBuf,'(A)')  ' '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT, 1 )
        ENDIF

       _END_MASTER( myThid )
       _BARRIER

      ENDIF

      call TIMER_STOP( 'I/O (WRITE)        [ADJOINT LOOP]', myThid )

#endif /* ALLOW_AUTODIFF_MONITOR */
#endif /* ALLOW_TANGENTLINEAR_RUN */

      end









