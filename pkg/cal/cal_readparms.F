C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_readparms.F,v 1.15 2014/05/27 23:41:30 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      SUBROUTINE CAL_READPARMS( myThid )

C     ==================================================================
C     SUBROUTINE cal_readparms
C     ==================================================================
C     o This routine initialises the calendar according to the user
C       specifications in "data.calendar".
C
C     started: Christian Eckert eckert@mit.edu  30-Jun-1999
C     changed: Christian Eckert eckert@mit.edu  29-Dec-1999
C              - restructured the original version in order to have a
C                better interface to the MITgcmUV.
C
C              Christian Eckert eckert@mit.edu  10-Jan-2000
C              - Modified namelist input. The data file is first copied
C                to scrunit1 with the comment lines being left out.
C                After this, scrunit1 is used to read the namelist data.
C
C              Christian Eckert eckert@mit.edu  19-Jan-2000
C              - Changed the role of the routine arguments. Chris Hill
C                proposed to make the calendar less "invasive". The tool
C                now assumes that the MITgcmUV already provides an ade-
C                quate set of time stepping parameters. The calendar
C                only associates a date with the given starttime of the
C                numerical model. startdate corresponds to zero start-
C                time. So, given niter0 or startdate .ne. zero the actual
C                startdate of the current integration is shifted by the
C                time interval correponding to niter0, startdate respec-
C                tively.
C
C              Christian Eckert eckert@mit.edu  03-Feb-2000
C              - Introduced new routine and function names, cal_<NAME>,
C                for verion 0.1.3.
C     ==================================================================
C     SUBROUTINE cal_readparms
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "cal.h"

C     == routine arguments ==
C     myThid       :: my Thread Id number
      INTEGER myThid

C     == local variables ==
      INTEGER       iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     == end of interface ==

C     Calendar parameters
      NAMELIST /CAL_NML/
     & TheCalendar,
     & startDate_1, startDate_2,
     & calendarDumps

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('CAL_READPARMS',myThid)
#endif

      IF ( .NOT.useCAL ) THEN
C-    pkg CAL is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
         cal_setStatus = -1
C     print a (weak) warning if data.cal is found
         CALL PACKAGES_UNUSED_MSG( 'useCAL', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C       Initialise the calendar parameters
        cal_setStatus = 0
        TheCalendar = ' '
        startdate_1 = 0
        startdate_2 = 0
        calendarDumps = .FALSE.

C       Next, read the calendar data file.
        WRITE(msgBuf,'(A)') 'CAL_READPARMS: opening data.cal'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

        CALL OPEN_COPY_DATA_FILE(
     I                          'data.cal', 'CAL_READPARMS',
     O                          iUnit,
     I                          myThid )
        READ(unit = iUnit, nml = cal_nml)
        WRITE(msgBuf,'(A)')
     &     'CAL_READPARMS: finished reading data.cal'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                SQUEEZE_RIGHT , 1)
        CLOSE( iUnit )

      _END_MASTER(myThid)

C     Everyone else must wait for the parameters to be loaded
      _BARRIER

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('CAL_READPARMS',myThid)
#endif

      RETURN
      END
