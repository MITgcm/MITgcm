C $Header: /u/gcmpack/MITgcm/pkg/autodiff/addummy_in_stepping.F,v 1.2 2001/04/10 22:35:26 heimbach Exp $

#include "CPP_OPTIONS.h"

      subroutine addummy_in_stepping( mytime, myiter, myThid )
      IMPLICIT NONE
C     /==========================================================\
C     | SUBROUTINE addummy_in_stepping                           |
C     |==========================================================|
C     == Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

#include "addynvars.h"
#include "adffields.h"

      common /adgrid_r/ addiffkr, adkapgm
      _RS addiffkr(1-olx:snx+olx,1-oly:sny+oly,1:nr,nsx,nsy)
      _RS adkapgm(1-olx:snx+olx,1-oly:sny+oly,1:nr,nsx,nsy)

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid
      integer myiter
      _RL     mytime 
C     suff - Hold suffix part of a filename
C     beginIOErrCount - Begin and end IO error counts
C     endIOErrCount
C     msgBuf - Error message buffer
      CHARACTER*(MAX_LEN_FNAM) suff
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf

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

        CALL WRITE_FLD_XY_RS ( 'ADJtaux.',suff, adfu, myIter, myThid)
        CALL WRITE_FLD_XY_RS ( 'ADJtauy.',suff, adfv, myIter, myThid)
        CALL WRITE_FLD_XYZ_RS ( 'ADJdiff.',suff, addiffkr, 
     &       myIter, myThid)
        CALL WRITE_FLD_XYZ_RS ( 'ADJkagm.',suff, adkapgm, 
     &       myIter, myThid)
        CALL WRITE_FLD_XY_RS ( 'ADJqnet.',suff, adqnet, myIter, myThid)
        CALL WRITE_FLD_XY_RS ('ADJempr.',suff, adempmr, myIter, myThid)
        CALL WRITE_FLD_XY_RS ( 'ADJ_sst.',suff, adsst, myIter, myThid)
        CALL WRITE_FLD_XY_RS ( 'ADJ_sss.',suff, adsss, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL ( 'ADJtemp.',suff, adgt, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL ( 'ADJsalt.',suff, adgs, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL ( 'ADJuvel.',suff, adgu, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL ( 'ADJvvel.',suff, adgv, myIter, myThid)

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









