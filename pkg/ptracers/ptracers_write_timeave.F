C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_write_timeave.F,v 1.10 2010/01/02 23:45:46 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP 1
C     !ROUTINE: PTRACERS_WRITE_TIMEAVE

C     !INTERFACE:
      SUBROUTINE PTRACERS_WRITE_TIMEAVE( myTime, myIter, myThid )

C     !DESCRIPTION:
C     At the end of average period, write the time-average
C     state-variables on file; then reset for next period

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_TAVE.h"

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !INPUT PARAMETERS:
C     myTime  :: Current time of simulation ( s )
C     myIter  :: Iteration number
C     myThid  :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#if ( defined ALLOW_PTRACERS && defined ALLOW_TIMEAVE )

C     !LOCAL VARIABLES:
C     suff    ::  Hold suffix part of a filename
      CHARACTER*(MAX_LEN_FNAM) suff
      INTEGER bi, bj, iTr
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_MNC
      INTEGER jj, ilnb
C     Functions
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      CHARACTER*(1) pf
#endif /*  ALLOW_MNC  */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     Final Time Averages and Dump Files if needed
      IF ( DIFFERENT_MULTIPLE( PTRACERS_taveFreq, myTime, deltaTClock )
     &   ) THEN

        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
C--         Normalize by integrated time
            DO iTr=1,PTRACERS_numInUse
              CALL TIMEAVE_NORMALIZE(
     &                     ptracerFluxtave(1-Olx,1-Oly,  1,1,iTr),
     &                     ptracer_full,  1, bi, bj, myThid )
              CALL TIMEAVE_NORMALIZE(
     &                     ptracertave    (1-Olx,1-Oly,1,1,1,iTr),
     &                     ptracer_half, Nr, bi, bj, myThid )
            ENDDO
          ENDDO
        ENDDO

C--     Write to files

#ifdef ALLOW_MNC
        IF ( PTRACERS_timeave_mnc ) THEN
          IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
            pf(1:1) = 'D'
          ELSE
            pf(1:1) = 'R'
          ENDIF
          CALL MNC_CW_SET_UDIM('ptr_tave', -1, myThid)
          CALL MNC_CW_RL_W_S('D','ptr_tave',0,0,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM('ptr_tave', 0, myThid)
          CALL MNC_CW_I_W_S('I','ptr_tave',0,0,'iter',myIter,myThid)

          CALL MNC_CW_SET_UDIM('ptr_flux_tave', -1, myThid)
          CALL MNC_CW_RL_W_S('D','ptr_flux_tave',0,0,'T',myTime,
     &         myThid)
          CALL MNC_CW_SET_UDIM('ptr_flux_tave', 0, myThid)
          CALL MNC_CW_I_W_S('I','ptr_flux_tave',0,0,'iter',myIter,
     &         myThid)

          DO iTr = 1,PTRACERS_numInUse
            CALL MNC_CW_RL_W(pf,'ptr_tave',0,0,PTRACERS_names(iTr),
     &           ptracertave(1-OLx,1-OLy,1,1,1,iTr),myThid)
            DO jj = 1,MAX_LEN_FNAM
              suff(jj:jj) = ' '
            ENDDO
            ilnb = ILNBLNK(PTRACERS_names(iTr))
            WRITE(suff,'(A,A)') 'surf_',PTRACERS_names(iTr)(1:ilnb)
            CALL MNC_CW_RL_W(pf,'ptr_flux_tave',0,0,suff,
     &           ptracerFluxtave(1-OLx,1-OLy,1,1,iTr),myThid)
          ENDDO
        ENDIF
#endif /*  ALLOW_MNC  */

        IF ( PTRACERS_timeave_mdsio ) THEN

C         Read IO error counter
          beginIOErrCount = IO_ERRCOUNT(myThid)

          DO iTr=1,PTRACERS_numInUse
            WRITE(suff,'(A7,I2.2,A1,I10.10)') 'PtrFluxtave',
     &                                         iTr, '.', myIter
            CALL WRITE_FLD_XY_RL ( suff, ' ',
     &                 ptracerFluxtave(1-Olx,1-Oly,  1,1,iTr),
     &                 myIter, myThid )
            WRITE(suff,'(A7,I2.2,A1,I10.10)') 'PTRtave',
     &                                         iTr, '.', myIter
            CALL WRITE_FLD_XYZ_RL( suff, ' ',
     &                 ptracertave    (1-Olx,1-Oly,1,1,1,iTr),
     &                 myIter, myThid )
          ENDDO

C         Reread IO error counter
          endIOErrCount = IO_ERRCOUNT(myThid)

C         Check for IO errors
          IF ( endIOErrCount .NE. beginIOErrCount ) THEN
C-          any thread that detects an error should report
            WRITE(msgBuf,'(A)')  'S/R PTRACERS_WRITE_TIMEAVE'
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A)')  'Error writing out data'
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A,I10)') 'Timestep ',myIter
            CALL PRINT_ERROR( msgBuf, myThid )
          ELSE
C-          normal case: 1 message is enough
            _BEGIN_MASTER( myThid )
            WRITE(msgBuf,'(A,I10)')
     &           '// PTRACER time-average data written, t-step', myIter
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT, myThid )
            WRITE(msgBuf,'(A)')  ' '
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT, myThid )
            _END_MASTER( myThid )
          ENDIF

        ENDIF

        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)

C           Like before the 1rst iteration,
C           ==> call TIMEAVE_TIMEAVE with myIter=nIter0 :
C           1) Reset the averages to zero ;
C           2) Start to cumulate state-variables with Half time step.

            CALL PTRACERS_TIMEAVE( myTime, nIter0, bi, bj, myThid )

          ENDDO
        ENDDO

      ENDIF

#endif /* ALLOW_PTRACERS and ALLOW_TIMEAVE */

      RETURN
      END
