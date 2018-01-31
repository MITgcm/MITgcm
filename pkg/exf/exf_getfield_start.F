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
C  | o old version of current S/R EXF_GETFFIELD_START
C  |   - with fewer arguments
C  |   - does not check if parameters are consistent with
C  |     useCAL setting
C  | o kept to allow to compile and use old piece of code
C  *==========================================================*
C \ev

C !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
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

C !LOCAL VARIABLES:
C     msgBuf          :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(3) pkg_name
      CHARACTER*(7) fld_name
      _RL fld_period
      INTEGER errCount
CEOP

      IF ( useCAL ) THEN
        pkg_name   = 'xxx'
        fld_name   = 'exf_gen'
        fld_period = oneRL
        fld_start_time = UNSET_RL
        errCount   = 0
        CALL EXF_GETFFIELD_START( useYearlyFields,
     I                     pkg_name, fld_name, fld_period,
     I                     fld_startdate1, fld_startdate2,
     U                     fld_start_time, errCount,
     I                     myThid )
      ENDIF

      RETURN
      END
