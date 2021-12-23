#include "EXF_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: EXF_GETFFIELD_START
C     !INTERFACE:
       SUBROUTINE EXF_GETFFIELD_START(
     I                useYearlyFields, pkg_name, fld_name,
     I                fld_period, fld_startdate1, fld_startdate2,
     U                fld_start_time, errCount,
     I                myThid )

C !DESCRIPTION: \bv
C  *==========================================================*
C  | SUBROUTINE EXF_GETFFIELD_START
C  | o get forcing-field starting-time (in secs);
C  |   distinguish between using Yearly-Fields or not.
C  *==========================================================*
C \ev

C !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "cal.h"
#include "PARAMS.h"

C !INPUT/OUTPUT PARAMETERS:
C     useYearlyFields :: select if using EXF Yearly-fields or not
C     pkg_name        :: package name from which this S/R is called
C     fld_name        :: field short name (to print mesg)
C     fld_period      :: time period (in sec) between 2 reccords
C     fld_startdate1  :: field starting date (YYYYMMDD)
C     fld_startdate2  :: field starting date (HHMMSS)
C     fld_start_time  :: corresponding starting time (in sec) for this field
C     errCount        :: error counter
C     myThid          :: My Thread Id number
      LOGICAL useYearlyFields
      CHARACTER*(*) pkg_name
      CHARACTER*(*) fld_name
      _RL fld_period
      INTEGER fld_startdate1, fld_startdate2
      _RL fld_start_time
      INTEGER errCount
      INTEGER myThid

C !FUNCTIONS:

C !LOCAL VARIABLES:
C     msgBuf          :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_CAL
      INTEGER date_array(4), difftime(4), yearStartDate(4)
      INTEGER gcm_startdate(4)
#endif /* ALLOW_CAL */
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Set default start_time (in case not using Calendar)
      IF ( fld_start_time.EQ.UNSET_RL ) THEN
        fld_start_time = 0.
      ELSEIF ( useCAL ) THEN
C-    Report inconsistent setting:
        WRITE(msgBuf,'(8A)') 'S/R EXF_GETFFIELD_START: ',
     &   'start-time for ', pkg_name, '-field "', fld_name,
     &    '" = ', fld_name, 'StartTime'
        CALL PRINT_ERROR( msgBuf, myThid )
c       WRITE(msgBuf,'(5A)') 'S/R EXF_GETFFIELD_START: ',
        WRITE(msgBuf,'(5A)') '    ',
     &   'is computed (useCAL) from startdate1 & date2',
     &   ' and cannot be set (in data.', pkg_name, ')'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

C--   Convert start-date to start_time (case using Calendar)
      IF ( useCAL .AND. (fld_period.GT.0. .OR.
     &     (fld_period.EQ.-1. .AND. .NOT.useYearlyFields)) ) THEN
#ifdef ALLOW_CAL
        CALL CAL_FULLDATE( fld_startdate1, fld_startdate2,
     &                     date_array, myThid )
        IF ( useYearlyFields ) THEN
          yearStartDate(1) = INT(date_array(1)/10000.) * 10000 + 101
          yearStartDate(2) = 0
          yearStartDate(3) = date_array(3)
          yearStartDate(4) = date_array(4)
          CALL cal_TimePassed( yearStartDate, date_array, difftime,
     I                         myThid )
          CALL cal_ToSeconds ( difftime, fld_start_time, myThid )
        ELSE
C--   with "cal.h" header file:
c         CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
c         CALL cal_ToSeconds ( difftime, fld_start_time, myThid )
c         fld_start_time = modelstart + fld_start_time
C--   with "PARAMS.h" header file:
          CALL cal_getdate( nIter0, startTime, gcm_startdate, myThid )
          CALL cal_TimePassed( gcm_startdate, date_array, difftime,
     I                         myThid )
          CALL cal_ToSeconds ( difftime, fld_start_time, myThid )
          fld_start_time = startTime  + fld_start_time
        ENDIF
#endif /* ALLOW_CAL */
      ELSEIF ( .NOT.useCAL ) THEN

       IF ( ( fld_startdate1.NE.0 .OR. fld_startdate2.NE.0 )
     &      .AND. fld_period.GT.0. ) THEN
C-    Report inconsistent setting:
        IF ( fld_startdate1.NE.0 ) THEN
         WRITE(msgBuf,'(8A)') 'S/R EXF_GETFFIELD_START: ',
     &   'start-date for ', pkg_name, '-field "', fld_name,
     &   '" = ', fld_name, 'startdate1'
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
        IF ( fld_startdate2.NE.0 ) THEN
         WRITE(msgBuf,'(8A)') 'S/R EXF_GETFFIELD_START: ',
     &   'start-date for ', pkg_name, '-field "', fld_name,
     &   '" = ', fld_name, 'startdate2'
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
c       WRITE(msgBuf,'(5A)') 'S/R EXF_GETFFIELD_START: ',
        WRITE(msgBuf,'(5A)') '    ',
     &   'is not allowed (in data.', pkg_name, ')',
     &   ' when pkg/cal is not used (useCAL=F)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF

       IF ( fld_period.LT.0. ) THEN
        WRITE(msgBuf,'(6A)') 'S/R EXF_GETFFIELD_START: ',
     &   'Invalid record period for ', pkg_name, '-field "',
     &   fld_name, '":'
        CALL PRINT_ERROR( msgBuf, myThid )
c       WRITE(msgBuf,'(3A,F14.2,A)') 'S/R EXF_GETFFIELD_START: ',
        WRITE(msgBuf,'(3A,F14.2,A)') '    ',
     &   fld_name, 'period =', fld_period,
     &   ' but should be >= 0 when useCAL=F'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF

C-    end if fld_period > 0
      ENDIF

      RETURN
      END
