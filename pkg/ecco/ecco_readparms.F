C $Header: /u/gcmpack/MITgcm/pkg/ecco/ecco_readparms.F,v 1.2 2001/07/13 13:42:16 heimbach Exp $

#include "CPP_OPTIONS.h"


      subroutine ecco_readparms(
     I                      mythid
     &                    )

c     ==================================================================
c     SUBROUTINE ecco_readparms
c     ==================================================================
c
c     o Initialize the ECCO release of the MITgcmUV.
c
c     started: Christian Eckert eckert@mit.edu 29-Feb-1999
c
c              - Introduced to create a package for the MITgcmUV.
c
c     changed: Christian Eckert eckert@mit.edu
c
c     ==================================================================
c     SUBROUTINE ecco_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ecco.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer errio
      integer il

      character*(max_len_prec) record

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c--   ECCO parameters.
      namelist /ECCO_PARMS/
     &                      expId
    
      _BEGIN_MASTER( myThid )
        open( unit=scrunit1, status='scratch' )

c--     Next, read the ECCO data file.
        open( unit   = modeldataunit,
     &        file   = 'data.ecco',
     &        status = 'old',
     &        iostat = errio)

        if ( errio .lt. 0 ) then
          stop ' stopped in ecco_readparms.'
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
        read  ( unit = scrunit1, nml = ecco_parms)
        close ( scrunit1 )

c-------------------------------- ECCO --------------------------------

#ifdef INCLUDE_ECCO_PACKAGE
C--   Initialise the ECCO State Estimation package.

#ifdef ALLOW_CALENDAR
C--   Initialise the calendar package.
#ifdef ALLOW_CAL_NENDITER
      call cal_init(
     I               startTime,
     I               endTime,
     I               deltaTclock,
     I               nIter0,
     I               nEndIter,
     I               nTimeSteps,
     I               myThid
     &             )
#else
      call cal_init(
     I               startTime,
     I               endTime,
     I               deltaTclock,
     I               nIter0,
     I               nTimeSteps,
     I               myThid
     &             )
#endif
      _BARRIER
#endif

C--   Custom routine to set forcing fields.
#ifdef INCLUDE_EXTERNAL_FORCING_PACKAGE
      call exf_readparms  ( mythid )
      call exf_init( mythid )
#endif /* INCLUDE_EXTERNAL_FORCING_PACKAGE */

#ifdef ALLOW_ECCO_OPTIMIZATION
c--   Initialise the optimization-related parts of the ECCO State
c--   Estimation package.
      call optim_init( myThid )
      _BARRIER
#else
c--   Optimization not enabled.
#endif

#ifndef ALLOW_ECCO_FORWARD_RUN
C--   Initialise the cost function.
      call cost_readparms( mythid )
      call cost_init( myThid )
      _BARRIER

C--   Initialise the control vector.
      call ctrl_init( myThid )
      _BARRIER
#else
c     not needed for forward run only
#endif

#ifdef ALLOW_ECCO_DIAGNOSTICS_RUN
c--   Initialise the ECCO diagnostics package.
      call diagnos_init( myThid )
      _BARRIER
#else
c--   Diagnostics not enabled.
#endif

#endif /* INCLUDE_ECCO_PACKAGE */

c-------------------------------- ECCO --------------------------------

c--     Summarize the setup of the ECCO release.
c        call ecco_summary( mythid )
      _END_MASTER( mythid )

c--   Everyone else must wait for the parameters to be loaded and
c--   the setup to be completed.
      _BARRIER

      end
