C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_statv_write.F,v 1.6 2004/09/03 20:10:47 edhill Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"
#ifdef ALLOW_GCHEM
# include "GCHEM_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: PTRACERS_STATV_WRITE

C     !INTERFACE:
      SUBROUTINE PTRACERS_STATV_WRITE(myTime, myIter, myThid)

C     !DESCRIPTION:
C     At the end of average period, write the time-average
C     state-variables on file; then reset for next period

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "DYNVARS.h"
c #include "GRID.h"
c #include "TIMEAVE_STATV.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#include "PTRACERS_STATV.h"
C     Functions
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !INPUT PARAMETERS:
C     myThid  ::  Thread number for this instance of the routine.
C     myIter  ::  Iteration number
C     myTime  ::  Current time of simulation ( s )
      INTEGER myThid
      INTEGER myIter
      _RL     myTime
CEOP

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_TIMEAVE 

C     !LOCAL VARIABLES:
C     suff    ::  Hold suffix part of a filename
C     TimeAve ::  total time over average
      INTEGER prevPrec
      CHARACTER*(MAX_LEN_FNAM) suff
      _RL DDTT
      INTEGER bi,bj,k, iTracer
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      
C     Final Time Averages and Dump Files if needed
c     IF ( myIter.NE.nIter0 .AND. DIFFERENT_MULTIPLE
c    &  (PTRACERS_taveFreq,myTime,myTime-deltaTClock) ) THEN
      IF ( DIFFERENT_MULTIPLE
     &     (PTRACERS_taveFreq, myTime, myTime-deltaTClock) ) THEN
        
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
C           Normalize by integrated time
            DO iTracer=1,PTRACERS_numInUse
              CALL TIMEAVE_NORMALIZ(
     &             ptracerFluxtave(1-Olx,1-Oly,  1,1,iTracer),
     &             ptracer_full, 1,bi,bj,myThid)
              CALL TIMEAVE_NORMALIZ(
     &             ptracertave    (1-Olx,1-Oly,1,1,1,iTracer),
     &             ptracer_half,Nr,bi,bj,myThid)
            ENDDO
          ENDDO
        ENDDO

C       Write to files
        _BARRIER
        _BEGIN_MASTER( myThid )


C       Set IO "context" for writing state
#ifdef USE_DFILE
        CALL DFILE_SET_RW
        CALL DFILE_SET_CONT_ON_ERROR
#endif
C       Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)
c       prevPrec = writeBinaryPrec
c       writeBinaryPrec = precFloat32
      
        do iTracer=1,PTRACERS_numInUse
          WRITE(suff,'(A7,I2.2,A1,I10.10)') 'PtrFluxtave',
     &         iTracer,'.',myIter
          CALL WRITE_FLD_XY_RL (suff,' ',
     &         ptracerFluxtave(1-Olx,1-Oly,  1,1,iTracer),
     &         myIter,myThid)
          WRITE(suff,'(A7,I2.2,A1,I10.10)') 'PTRtave',
     &         iTracer,'.',myIter
          CALL WRITE_FLD_XYZ_RL(suff,' ',
     &         ptracertave    (1-Olx,1-Oly,1,1,1,iTracer),
     &         myIter,myThid)
        enddo


c       writeBinaryPrec = prevPrec
C       Reread IO error counter
        endIOErrCount = IO_ERRCOUNT(myThid)

C       Check for IO errors
        IF ( endIOErrCount .NE. beginIOErrCount ) THEN
          WRITE(msgBuf,'(A)')  'S/R WRITE_TIME_AVERAGES'
          CALL PRINT_ERROR( msgBuf, 1 )
          WRITE(msgBuf,'(A)')  'Error writing out data'
          CALL PRINT_ERROR( msgBuf, 1 )
          WRITE(msgBuf,'(A,I10)') 'Timestep ',myIter
          CALL PRINT_ERROR( msgBuf, 1 )
        ELSE
          WRITE(msgBuf,'(A,I10)')  
     &         '// Time-average data written, t-step', myIter
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &         SQUEEZE_RIGHT, 1 )
          WRITE(msgBuf,'(A)')  ' '
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &         SQUEEZE_RIGHT, 1 )
        ENDIF
        
        _END_MASTER( myThid )
        _BARRIER
        
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)

C           Like before the 1rst iteration,
C           ==> call TIMEAVE_STATVARS with myIter=nIter0 :
C           1) Reset the averages to zero ;
C           2) Start to cumulate state-variables with Half time step.

            CALL PTRACERS_STATVARS(myTime, nIter0, bi, bj, myThid)

          ENDDO
        ENDDO

      ENDIF
 
#endif /* ALLOW_TIMEAVE */
#endif /* ALLOW_PTRACERS */

      RETURN
      END
