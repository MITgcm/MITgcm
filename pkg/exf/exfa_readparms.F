c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exfa_readparms.F,v 1.1 2001/05/14 22:08:42 heimbach Exp $

#include "EXF_CPPOPTIONS.h"


      subroutine exfa_readparms(
     I                           mythid
     &                         )

c     ==================================================================
c     SUBROUTINE exfa_readparms
c     ==================================================================
c
c     o This routine initialises the package that calculates external
c       forcing field anomalies for a given timestep of the MITgcmUV. Parameters
c       for this package are set in "data.exfa". Some additional
c       precompiler switches have to be specified in "EXF_CPPOPTIONS.h" .
c
c     started: Ralf.Giering@FastOpt.de 24-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exfa_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "cal.h"
#include "exf_constants.h"

#include "exfa.h"
#include "exfa_param.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer i

c     Surface flux data.
      namelist /exfa_nml/
     &    hfluxastartdate1,    hfluxastartdate2,   hfluxaperiod,
     &    sfluxastartdate1,    sfluxastartdate2,   sfluxaperiod,
     &  ustressastartdate1,  ustressastartdate2, ustressaperiod,
     &  vstressastartdate1,  vstressastartdate2, vstressaperiod,
     &          hfluxafile,          sfluxafile,
     &        ustressafile,        vstressafile,
     &           exf_iprec,          exf_yftype

c     == end of interface ==

      _BEGIN_MASTER(mythid)

c     Set default values.

c     Calendar data.
      hfluxastartdate1    = 0
      hfluxastartdate2    = 0
      hfluxaperiod        = 0.0 _d 0

      sfluxastartdate1    = 0
      sfluxastartdate2    = 0
      sfluxaperiod        = 0.0 _d 0

      ustressastartdate1  = 0
      ustressastartdate2  = 0
      ustressaperiod      = 0.0 _d 0

      vstressastartdate1  = 0
      vstressastartdate2  = 0
      vstressaperiod      = 0.0 _d 0

c     Data files.
      hfluxafile          = ' '
      sfluxafile          = ' '
      ustressafile        = ' '
      vstressafile        = ' '

c     Initialise the date arrays.
      do i = 1,4
         hfluxastartdate(i)    = 0
         sfluxastartdate(i)    = 0
         ustressastartdate(i)  = 0
         vstressastartdate(i)  = 0
      enddo

c     Initialise file type and field precision
      exf_iprec = 32
      exf_yftype = 'RL'

c     Check for the availability of the right calendar version.
      if ( calendarversion .ne. use_cal_version ) then
         print*,' exfa_Init: You are not using the appropriate'
         print*,'            version of the calendar package.'
         print*
         print*,' Please use Calendar version: ', use_cal_version
         stop ' stopped in exf_Init.'
      endif

c     Next, read the forcing data file.
      call nml_filter( 'data.exfa', scrunit1, myThid )
      if (scrunit1 .eq. 0) then
         stop 'exfa_readparms: reading namelist failed'
      end if
      read(  scrunit1, nml = exfa_nml )
      close( scrunit1 )

c     Complete the start date specifications for the forcing
c     fields to get a complete calendar date array.

      call cal_FullDate(   hfluxastartdate1,  hfluxastartdate2,
     &                     hfluxastartdate,           mythid )
      call cal_FullDate(   sfluxastartdate1,  sfluxastartdate2,
     &                     sfluxastartdate,           mythid )
      call cal_FullDate( ustressastartdate1, ustressastartdate2,
     &                   ustressastartdate,            mythid )
      call cal_FullDate( vstressastartdate1, vstressastartdate2,
     &                   vstressastartdate,            mythid )

      _END_MASTER( mythid )

      _BARRIER

c     Summarize the External forcing's setup.
      call exfa_Summary( mythid )

      end
