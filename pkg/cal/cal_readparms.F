C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_readparms.F,v 1.6 2005/05/23 19:28:44 heimbach Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      subroutine cal_readparms( mythid )

c     ==================================================================
c     SUBROUTINE cal_readparms
c     ==================================================================
c
c     o This routine initialises the calendar according to the user
c       specifications in "data.calendar".
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  29-Dec-1999
c
c              - restructured the original version in order to have a
c                better interface to the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  10-Jan-2000
c
c              - Modified namelist input. The data file is first copied
c                to scrunit1 with the comment lines being left out.
c                After this, scrunit1 is used to read the namelist data.
c
c              Christian Eckert eckert@mit.edu  19-Jan-2000
c
c              - Changed the role of the routine arguments. Chris Hill
c                proposed to make the calendar less "invasive". The tool
c                now assumes that the MITgcmUV already provides an ade-
c                quate set of time stepping parameters. The calendar
c                only associates a date with the given starttime of the
c                numerical model. startdate corresponds to zero start-
c                time. So, given niter0 or startdate .ne. zero the actual
c                startdate of the current integration is shifted by the
c                time interval correponding to niter0, startdate respec-
c                tively.
c
c              Christian Eckert eckert@mit.edu  03-Feb-2000
c
c              - Introduced new routine and function names, cal_<NAME>,
c                for verion 0.1.3.
c
c     ==================================================================
c     SUBROUTINE cal_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

c     == routine arguments ==

c     modstart        - start time of the model integration
c     modend          - end time of the model integration
c     moditerini      - initial iteration number of the model
c     moditerend      - last iteration number of the model
c     modstep         - timestep of the numerical model
c     modintsteps     - number of timesteps that are to be performed.
c     mythid          - number of this instance of the subrotuine.

      _RL     modstart
      _RL     modend
      _RL     modstep
      integer moditerini
      integer moditerend
      integer modintsteps
      integer mythid

c     == local variables ==

      integer errio
      integer ierr
      integer il
      integer enddate_1
      integer enddate_2

c     Variables related to the calendar tool.
      character*(9) TheCalendar
      integer       startdate_1
      integer       startdate_2
      integer       iUnit

      character*(max_len_mbuf) msgbuf
      character*(max_len_prec) record

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c     Calendar parameters
c     (version 0.1.3 >> START << )

      namelist /CAL_NML/
     & TheCalendar,
     & startDate_1,startDate_2

c     (version 0.1.3 >> END << )


      _BEGIN_MASTER(myThid)

c       Initialise the calendar parameters
        TheCalendar = ' '
        startdate_1 = 0
        startdate_2 = 0
        enddate_1   = 0
        enddate_2   = 0

        open(unit=scrunit1,status='scratch')

c       Next, read the calendar data file.
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

c       Check consistency of the date specifications with the MITgcm
c       startTime, endTime, and nTimeSteps, then set all the calendar
c       parameters.

        modstart    = startTime
        modend      = endTime
        modstep     = deltaTclock
        moditerini  = nIter0
        moditerend  = nEndIter
        modintsteps = nTimeSteps

        call cal_Set(
     I                modstart,
     I                modend,
     I                modstep,
     I                TheCalendar,
     I                startdate_1,
     I                startdate_2,
     I                enddate_1,
     I                enddate_2,
     I                moditerini,
     I                moditerend,
     I                modintsteps,
     I                mythid
     &              )

        call cal_Summary( mythid )

      _END_MASTER(myThid)

c     Everyone else must wait for the parameters to be loaded
      _BARRIER

      return
      end
