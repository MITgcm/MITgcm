cswdptr -- added routine ---
#include "CPP_OPTIONS.h"
#include "PTRACERS_OPTIONS.h"
#ifdef ALLOW_GCHEM
# include "GCHEM_OPTIONS.h"
#endif

      SUBROUTINE PTRACERS_STATV_WRITE(myTime, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE PTRACERS_STATV_WRITE                           |
C     | o At the end of average period, write the time-average   |
C     |   state-variables on file ; then reset for next period   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "TIMEAVE_STATV.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS.h"
# include "PTRACERS_STATV.h"
#endif

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

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_TIMEAVE 

C     == Local variables ==
C     suff - Hold suffix part of a filename
C     TimeAve - total time over average
      INTEGER prevPrec
      CHARACTER*(MAX_LEN_FNAM) suff
      _RL DDTT
      INTEGER bi,bj,k, iTracer
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf

 
C-    Final Time Averages and Dump Files if needed
c     IF ( myIter.NE.nIter0 .AND. 
c    &  DIFFERENT_MULTIPLE(taveFreq,myTime,myTime-deltaTClock) ) THEN
      IF(DIFFERENT_MULTIPLE(taveFreq,myTime,myTime-deltaTClock) )THEN



       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
C-     Normalize by integrated time
         do iTracer=1,PTRACERS_numInUse
          CALL TIMEAVE_NORMALIZ(ptracertave(1-Olx,1-Oly,1,1,1,iTracer),
     &                           ptracer_half,Nr,bi,bj,myThid)
         enddo
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
C--    Read IO error counter
       beginIOErrCount = IO_ERRCOUNT(myThid)
c      prevPrec = writeBinaryPrec
c      writeBinaryPrec = precFloat32
      

       do iTracer=1,PTRACERS_numInUse
         WRITE(suff,'(A7,I2.2,A1,I10.10)') 'PTRtave',
     &         iTracer,'.',myIter
     

         CALL WRITE_FLD_XYZ_RL(suff,' ',
     &      ptracertave(1-Olx,1-Oly,1,1,1,iTracer),myIter,myThid)
       enddo


c      writeBinaryPrec = prevPrec
C--    Reread IO error counter
       endIOErrCount = IO_ERRCOUNT(myThid)

C--    Check for IO errors
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

C-- Like before the 1rst iteration,
C  ==> call TIMEAVE_STATVARS with myIter=nIter0 :
C    1) Reset the averages to zero ;
C    2) Start to cumulate state-variables with Half time step.

         CALL PTRACERS_STATVARS(myTime, nIter0, bi, bj, myThid)

        ENDDO
       ENDDO

      ENDIF
 
#endif /* ALLOW_TIMEAVE */
#endif

      RETURN
      END
