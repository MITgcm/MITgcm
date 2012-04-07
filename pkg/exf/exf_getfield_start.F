C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_getfield_start.F,v 1.1 2012/04/07 20:06:52 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: EXF_GETFIELD_START
C     !INTERFACE:
       SUBROUTINE EXF_GETFIELD_START(
     I                useYearlyFields,
     I                fld_startdate1, fld_startdate2,
     O                fld_start_time,
     I                myThid )

C !DESCRIPTION: \bv
C  *==========================================================*
C  | SUBROUTINE EXF_GETFIELD_START
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
C     fld_startdate1  :: field starting date (YYYYMMDD)
C     fld_startdate2  :: field starting date (HHMMSS)
C     fld_start_time  :: corresponding starting time (in sec) for this field
C     myThid          :: My Thread Id number
      LOGICAL useYearlyFields
      INTEGER fld_startdate1, fld_startdate2
      _RL fld_start_time
      INTEGER myThid

C !FUNCTIONS:

C !LOCAL VARIABLES:
C     msgBuf          :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER date_array(4), difftime(4), yearStartDate(4)
      INTEGER gcm_startdate(4)
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      CALL CAL_FULLDATE( fld_startdate1, fld_startdate2,
     &                   date_array, myThid )
      IF ( useYearlyFields ) THEN
        yearStartDate(1) = INT(date_array(1)/10000.) * 10000 + 101
        yearStartDate(2) = 0
        yearStartDate(3) = date_array(3)
        yearStartDate(4) = date_array(4)
        CALL cal_TimePassed( yearStartDate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime, fld_start_time, myThid)
      ELSE
C--   with "cal.h" header file:
c       CALL cal_TimePassed(modelstartdate,date_array,difftime,myThid)
c       CALL cal_ToSeconds ( difftime, fld_start_time, myThid)
c       fld_start_time = modelstart + fld_start_time
C--   with "PARAMS.h" header file:
        CALL cal_getdate( nIter0, startTime, gcm_startdate, myThid )
        CALL cal_TimePassed( gcm_startdate,date_array,difftime,myThid)
        CALL cal_ToSeconds ( difftime, fld_start_time, myThid)
        fld_start_time = startTime  + fld_start_time
      ENDIF

      RETURN
      END
