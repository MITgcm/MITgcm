C $Header: /u/gcmpack/MITgcm/pkg/autodiff/addummy_in_stepping.F,v 1.7 2001/11/20 23:27:29 heimbach Exp $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: addummy_in_stepping
C     !INTERFACE:
      subroutine addummy_in_stepping( mytime, myiter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE addummy_in_stepping                           |
C     *==========================================================*
C     Extract adjoint variable from TAMC/TAF-generated
C     adjoint common blocks, contained in adcommon.h
C     and write fields to file;
C     Make sure common blocks in adcommon.h are up-to-date
C     w.r.t. current adjoint code.
C     *==========================================================*
C     | SUBROUTINE addummy_in_stepping                           |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "adcommon.h"

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
     &  DIFFERENT_MULTIPLE(dumpFreq,mytime,
     &                     mytime-deltaTClock)
     & ) THEN

         write(*,*) 'myIter= ',myiter

       _BARRIER
       _BEGIN_MASTER( myThid )

C--     Set suffix for this set of data files.
        WRITE(suff,'(I10.10)') myIter
        writeBinaryPrec = writeStatePrec

C--     Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)

        CALL WRITE_FLD_XY_RL ( 'ADJtaux.',suff, adfu, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJtauy.',suff, adfv, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJqnet.',suff, adqnet, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'ADJempr.',suff, adempmr, myIter, myThid)
c
        CALL WRITE_FLD_XYZ_RL( 
     &       'ADJgtnm1.',suff, adgtnm1, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL( 
     &       'ADJgsnm1.',suff, adgsnm1, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL( 
     &       'ADJgunm1.',suff, adgunm1, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL( 
     &       'ADJgvnm1.',suff, adgvnm1, myIter, myThid)
#ifdef ALLOW_PASSIVE_TRACER
        CALL WRITE_FLD_XYZ_RL( 
     &       'ADJgtr1nm1. ',suff, adgtr1nm1, myIter, myThid)
#endif
c
        CALL WRITE_FLD_XYZ_RL( 
     &       'ADJtheta.',suff, adtheta, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL( 
     &       'ADJsalt.',suff, adsalt, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL( 
     &       'ADJuvel.',suff, aduvel, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL( 
     &       'ADJvvel.',suff, advvel, myIter, myThid)

#ifdef ALLOW_PASSIVE_TRACER
        CALL WRITE_FLD_XYZ_RL( 
     &       'ADJtr1. ',suff, adtr1, myIter, myThid)
#endif
#ifdef ALLOW_DIFFKR_CONTROL
        CALL WRITE_FLD_XYZ_RL ( 'ADJdiffkr.',suff, addiffkr, 
     &       myIter, myThid)
#endif
#ifdef ALLOW_KAPGM_CONTROL
        CALL WRITE_FLD_XYZ_RL ( 'ADJkagm.',suff, adkapgm, 
     &       myIter, myThid)
#endif

cph        CALL WRITE_FLD_XY_RL( 'ADJ_sst.',suff, adsst, myIter, myThid)
cph        CALL WRITE_FLD_XY_RL( 'ADJ_sss.',suff, adsss, myIter, myThid)

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
      end









