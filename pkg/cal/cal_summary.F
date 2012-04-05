C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_summary.F,v 1.5 2012/04/05 21:59:03 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      SUBROUTINE CAL_SUMMARY(
     I                        myThid )

C     ==================================================================
C     SUBROUTINE cal_Summary
C     ==================================================================
C
C     o List all the settings of the calendar tool.
C
C     started: Christian Eckert eckert@mit.edu  30-Jun-1999
C     changed: Christian Eckert eckert@mit.edu  10-Jan-2000
C              - corrected typo: nIterEnd --> nEndIter.
C              Christian Eckert eckert@mit.edu  03-Feb-2000
C              - Introduced new routine and function names, cal_<NAME>,
C                for verion 0.1.3.
C
C     ==================================================================
C     SUBROUTINE cal_Summary
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "cal.h"

C     == routine arguments ==
C     myThid  - thread number for this instance of the routine.
      INTEGER myThid

C     == local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER numcaldays
      INTEGER numcalmonths
      INTEGER numcalyears

C     == EXTERNAL ==
      INTEGER  cal_IntYears
      EXTERNAL cal_IntYears
      INTEGER  cal_IntMonths
      EXTERNAL cal_IntMonths
      INTEGER  cal_IntDays
      EXTERNAL cal_IntDays

C     == end of interface ==

      _BEGIN_MASTER(myThid)

      numcalyears  = cal_IntYears ( myThid )
      numcalmonths = cal_IntMonths( myThid )
      numcaldays   = cal_IntDays  ( myThid )

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// Calendar configuration >>> START <<<'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
c     WRITE(msgBuf,'(A,A)')
c    &'Calendar version: ',calendarversion
c     CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
c    &                    SQUEEZE_RIGHT, myThid )
c     WRITE(msgBuf,'(A)') ' '
c     CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
c    &                    SQUEEZE_RIGHT, myThid )

      CALL WRITE_0D_RL( modelstart, INDEX_NONE,'modelstart =',
     &  ' /* Start time of the model integration [s] */')
      CALL WRITE_0D_RL( modelend, INDEX_NONE, 'modelend  =',
     &  ' /* End time of the model integration [s] */')
      CALL WRITE_0D_RL( modelstep, INDEX_NONE,'modelstep =',
     &  ' /* Time interval for a model forward step [s] */')
      CALL WRITE_0D_L( usingGregorianCalendar, INDEX_NONE,
     &                'usingGregorianCalendar=',
     &  ' /* Calendar Type: Gregorian Calendar */')
      CALL WRITE_0D_L( usingJulianCalendar, INDEX_NONE,
     &                'usingJulianCalendar =',
     &  ' /* Calendar Type: Julian Calendar */')
      CALL WRITE_0D_L( usingModelCalendar, INDEX_NONE,
     &                'usingModelCalendar  =',
     &  ' /* Calendar Type: Model Calendar */')
      CALL WRITE_0D_I( modelstartdate(1), INDEX_NONE,
     &                'modelstartdate YYYYMMDD =',
     &  ' /* Model start date YYYY-MM-DD */')
      CALL WRITE_0D_I( modelstartdate(2), INDEX_NONE,
     &                '  modelstartdate HHMMSS =',
     &  ' /* Model start date HH-MM-SS  */')
      CALL WRITE_0D_I( modelenddate(1), INDEX_NONE,
     &                'modelenddate   YYYYMMDD =',
     &  ' /* Model end date YYYY-MM-DD */')
      CALL WRITE_0D_I( modelenddate(2), INDEX_NONE,
     &                '  modelenddate   HHMMSS =',
     &  ' /* Model end date HH-MM-SS  */')
      CALL WRITE_0D_I( numcalyears, INDEX_NONE,
     &                'intyears =',
     &  ' /* Number of calendar years affected by the integration */')
      CALL WRITE_0D_I( numcalmonths, INDEX_NONE,
     &                'intmonths=',
     &  ' /* Number of calendar months affected by the integration */')
      CALL WRITE_0D_I( numcaldays, INDEX_NONE,
     &                'intdays =',
     &  ' /* Number of calendar days affected by the integration */')
      CALL WRITE_0D_I( modeliter0, INDEX_NONE,
     &                'modeliter0 =',
     &  ' /* Base timestep number  */')
      CALL WRITE_0D_I( modeliterend, INDEX_NONE,
     &                'modeliterend =',
     &  ' /* Final timestep number  */')
      CALL WRITE_0D_I( modelintsteps, INDEX_NONE,
     &                'modelintsteps=',
     &  ' /* Number of model timesteps  */')
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// Calendar configuration  >>> END <<<'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      _END_MASTER(myThid)

      RETURN
      END
