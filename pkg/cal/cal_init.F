C $Header: /u/gcmpack/MITgcm/pkg/cal/Attic/cal_init.F,v 1.1 2001/05/14 22:07:26 heimbach Exp $

#include "CAL_CPPOPTIONS.h"

#ifdef ALLOW_CAL_NENDITER
      subroutine cal_Init(
     I                     modstart,
     I                     modend,
     I                     modstep,
     I                     moditerini,
     I                     moditerend,
     I                     modintsteps,
     I                     mythid
     &                   )
#else
      subroutine cal_Init(
     I                     modstart,
     I                     modend,
     I                     modstep,
     I                     moditerini,
     I                     modintsteps,
     I                     mythid
     &                   )
#endif

c     ==================================================================
c     SUBROUTINE cal_Init
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
c     SUBROUTINE cal_Init
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"

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
#ifdef ALLOW_CAL_NENDITER
      integer moditerend
#endif
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

c       Initialise the calendar's parameters
        TheCalendar = ' '
        startdate_1 = 0
        startdate_2 = 0
        enddate_1   = 0
        enddate_2   = 0

        open(unit=scrunit1,status='scratch')

c       Next, read the calendar data file.
        open(unit = modeldataunit,file = 'data.cal',
     &       status = 'old', iostat = errio)
        if ( errio .lt. 0 ) then
          ierr = 1
          call cal_PrintError( ierr, mythid )
          stop ' stopped in cal_Init.'
        endif

        do while ( .true. )
          read(modeldataunit, fmt='(a)', end=1001) record
          il = max(ilnblnk(record),1)
          if ( record(1:1) .ne. commentcharacter )
     &        write(unit=scrunit1, fmt='(a)') record(:il)
        enddo
 1001   continue

        close( modeldataunit )

        rewind( scrunit1 )
        read(unit = scrunit1, nml = cal_nml)
        close( scrunit1 )


c       Check consistency of the date specifications with the MITgcmUV's
c       startTime, endTime, and nTimeSteps, then set all the calendar
c       parameters.

#ifdef ALLOW_CAL_NENDITER
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
#else
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
     I                modintsteps,
     I                mythid
     &              )
#endif

        call cal_Summary( mythid )

      _END_MASTER(myThid)

c     Everyone else must wait for the parameters to be loaded
      _BARRIER

      return
      end
