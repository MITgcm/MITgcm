C $Header: /u/gcmpack/MITgcm/diags/src/Attic/write_time_averages.F,v 1.14 2001/02/15 19:01:39 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

      SUBROUTINE WRITE_TIME_AVERAGES(myTime, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE WRITE_TIME_AVERAGES                           |
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AVER.h"
#include "DYNVARS.h"

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
 
C     Final Time Averages and Dump Files if needed
      IF ( myIter.NE.nIter0 .AND. 
     &  DIFFERENT_MULTIPLE(taveFreq,myTime,myTime-deltaTClock) ) THEN

       WRITE(suff,'(I10.10)') myIter

C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          CALL TIMEAVER_FINAL_XYZ(thetatave,TimeAve_half,bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(salttave,TimeAve_half,bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(uVeltave,TimeAve_half,bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(vVeltave,TimeAve_half,bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(uttave,TimeAve_half,bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(vttave,TimeAve_half,bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(wVeltave,TimeAve_full,bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(phiHydtave,TimeAve_full,
     &                            bi,bj,K,myThid)
          CALL TIMEAVER_FINAL_XYZ(ConvectCountTave,TimeAve_full,
     &                            bi,bj,K,myThid)
         ENDDO
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
         DO k=1,Nr

C      Reset the averages to zero
          CALL TIMEAVER_INI_XYZ(thetatave, bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(salttave, bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(uVeltave, bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(vVeltave, bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(wVeltave, bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(uttave, bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(vttave, bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(phiHydtave, bi,bj,K,myThid)
          CALL TIMEAVER_INI_XYZ(ConvectCountTave, bi,bj,K,myThid)
          TimeAve_half(k,bi,bj)=0.
          TimeAve_full(k,bi,bj)=0.

C      Time Averages of single fields
          DDTT=0.5*deltaTclock
          CALL TIMEAVER_1FLD_XYZ(theta,thetatave,DDTT, bi,bj,K,myThid)
          CALL TIMEAVER_1FLD_XYZ(salt, salttave, DDTT, bi,bj,K,myThid)
          CALL TIMEAVER_1FLD_XYZ(uVel, uVeltave, DDTT, bi,bj,K,myThid)
          CALL TIMEAVER_1FLD_XYZ(vVel, vVeltave, DDTT, bi,bj,K,myThid)

C      Time Averages of "double" fields
          CALL TIMEAVER_2FLD_XYZ(theta, uVel, uttave, 1, DDTT, 
     &         bi,bj,K,myThid)
          CALL TIMEAVER_2FLD_XYZ(theta, vVel, vttave, 2, DDTT, 
     &         bi,bj,K,myThid)

C      Keep record of how much time has been integrated over
          TimeAve_half(k,bi,bj)=TimeAve_half(k,bi,bj)+DDTT

         ENDDO
        ENDDO
       ENDDO

      ENDIF
 
      RETURN
      END
