C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_statv_write.F,v 1.1 2001/03/06 15:56:08 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

      SUBROUTINE TIMEAVE_STATV_WRITE(myTime, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE TIMEAVE_STATV_WRITE                           |
C     | o At the end of average period, write the time-average   |
C     |   state-variables on file ; then reset for next period   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "TIMEAVE_STATV.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myTime - Current time of simulation ( s )
      INTEGER myThid
      INTEGER myIter
      _RL     myTime

C     == Local variables ==
C     suff - Hold suffix part of a filename
C     TimeAve - total time over average
      INTEGER prevPrec
      CHARACTER*(MAX_LEN_FNAM) suff
      _RL DDTT
      INTEGER bi,bj,k
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf
 
C-    Final Time Averages and Dump Files if needed
      IF ( myIter.NE.nIter0 .AND. 
     &  DIFFERENT_MULTIPLE(taveFreq,myTime,myTime-deltaTClock) ) THEN

       WRITE(suff,'(I10.10)') myIter

C-     Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_NORMALIZ(etaTave,  TimeAve_half,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(thetatave,TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(salttave, TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(uVeltave, TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(vVeltave, TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(uttave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(vttave,   TimeAve_half,Nr,bi,bj,myThid)

          CALL TIMEAVE_NORMALIZ(wVeltave, TimeAve_full,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(phiHydtave,TimeAve_full,Nr,
     &                            bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(ConvectCountTave,TimeAve_full,Nr,
     &                            bi,bj,myThid)
        ENDDO
       ENDDO

C      Write to files
       _BARRIER
       _BEGIN_MASTER( myThid )

C--     Set IO "context" for writing state
#ifdef USE_DFILE
        CALL DFILE_SET_RW
        CALL DFILE_SET_CONT_ON_ERROR
#endif
C--     Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)
        prevPrec = writeBinaryPrec
        writeBinaryPrec = precFloat32
        CALL WRITE_FLD_XY_RL('ETAtave.',suff,etaTave,myIter,myThid)

        CALL WRITE_FLD_XYZ_RL('Ttave.',suff,thetatave,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('Stave.',suff,salttave,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('uVeltave.',suff,uVeltave,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('vVeltave.',suff,vVeltave,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('UTtave.',suff,uttave,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('VTtave.',suff,vttave,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('wVeltave.',suff,wVeltave,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('PhHytave.',suff,phiHydtave,myIter,myThid)
        CALL WRITE_FLD_XYZ_RL('Convtave.',suff,ConvectCountTave,
     &                        myIter,myThid)
        writeBinaryPrec = prevPrec
C--     Reread IO error counter
        endIOErrCount = IO_ERRCOUNT(myThid)

C--     Check for IO errors
        IF ( endIOErrCount .NE. beginIOErrCount ) THEN
         WRITE(msgBuf,'(A)')  'S/R WRITE_TIME_AVERAGES'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A)')  'Error writing out data'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A,I10)') 'Timestep ',myIter
         CALL PRINT_ERROR( msgBuf, 1 )
        ELSE
         WRITE(msgBuf,'(A,I10)')  
     &    '// Time-average data written, t-step', myIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT, 1 )
         WRITE(msgBuf,'(A)')  ' '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT, 1 )
        ENDIF

       _END_MASTER( myThid )
       _BARRIER

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)

C--    Reset the averages to zero
          CALL TIMEAVE_RESET(etaTave,  1 , bi,bj,myThid)
          CALL TIMEAVE_RESET(thetatave,Nr, bi,bj,myThid)
          CALL TIMEAVE_RESET(salttave, Nr, bi,bj,myThid)
          CALL TIMEAVE_RESET(uVeltave, Nr, bi,bj,myThid)
          CALL TIMEAVE_RESET(vVeltave, Nr, bi,bj,myThid)
          CALL TIMEAVE_RESET(wVeltave, Nr, bi,bj,myThid)
          CALL TIMEAVE_RESET(uttave,   Nr, bi,bj,myThid)
          CALL TIMEAVE_RESET(vttave,   Nr, bi,bj,myThid)
          CALL TIMEAVE_RESET(phiHydtave,Nr,bi,bj,myThid)
          CALL TIMEAVE_RESET(ConvectCountTave,Nr,bi,bj,myThid)
          DO k=1,Nr
            TimeAve_half(k,bi,bj)=0.
            TimeAve_full(k,bi,bj)=0.
          ENDDO

C--    Start to cumulate state-variables with Half time step :
          DDTT=0.5*deltaTclock
C      "single" fields
          CALL TIMEAVE_CUMULATE(etaTave,  etaN, 1, DDTT, bi,bj,myThid)
          CALL TIMEAVE_CUMULATE(thetatave,theta,Nr,DDTT, bi,bj,myThid)
          CALL TIMEAVE_CUMULATE(salttave, salt, Nr,DDTT, bi,bj,myThid)
          CALL TIMEAVE_CUMULATE(uVeltave, uVel, Nr,DDTT, bi,bj,myThid)
          CALL TIMEAVE_CUMULATE(vVeltave, vVel, Nr,DDTT, bi,bj,myThid)

C      "double" fields
          CALL TIMEAVE_CUMUL_2V(uttave, theta, uVel, Nr, 1, DDTT, 
     &         bi,bj,myThid)
          CALL TIMEAVE_CUMUL_2V(vttave, theta, vVel, Nr, 2, DDTT, 
     &         bi,bj,myThid)

C-     Keep record of how much time has been integrated over
          DO k=1,Nr
            TimeAve_half(k,bi,bj)=TimeAve_half(k,bi,bj)+DDTT
          ENDDO

        ENDDO
       ENDDO

      ENDIF
 
      RETURN
      END
