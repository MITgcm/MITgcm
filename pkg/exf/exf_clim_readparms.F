C $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_clim_readparms.F,v 1.15 2007/04/18 05:02:08 heimbach Exp $
C $Name:  $

#include "EXF_OPTIONS.h"


      subroutine exf_clim_readparms(
     I                               mythid
     &                             )

c     ==================================================================
c     SUBROUTINE exf_clim_readparms
c     ==================================================================
c
c     o This routine initialises the climatologic forcing
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-20000
c
c     ==================================================================
c     SUBROUTINE exf_clim_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "cal.h"
#include "EXF_PARAM.h"
#include "EXF_CLIM_PARAM.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer i
      integer date_array(4), difftime(4)
      integer iUnit

      character*(max_len_mbuf) msgbuf

c     == end of interface ==

c     Surface flux data.
      namelist /exf_clim_nml/
     &     climtempfreeze,
     & climtempstartdate1, climtempstartdate2, climtempperiod,
     & climsaltstartdate1, climsaltstartdate2, climsaltperiod,
     &  climsststartdate1,  climsststartdate2,  climsstperiod,
     &  climsssstartdate1,  climsssstartdate2,  climsssperiod,
     &       climtempfile,       climsaltfile,    climsstfile,
     &        climsssfile,       climsstconst,   climsssconst,
     & exf_inscal_climsst, exf_inscal_climsss,
     &     exf_clim_iprec, exf_clim_yftype
#ifdef USE_EXF_INTERPOLATION
     & ,climsst_lon0, climsst_lon_inc,
     & climsst_lat0, climsst_lat_inc,
     & climsst_nlon, climsst_nlat,
     & climsss_lon0, climsss_lon_inc,
     & climsss_lat0, climsss_lat_inc,
     & climsss_nlon, climsss_nlat
#endif

      _BEGIN_MASTER(mythid)

c     Set default values.

c     Calendar data.
      climsststartdate1  = 0
      climsststartdate2  = 0
      climsstperiod      = 0
      climsstconst         = 0.0 _d 0
      climsst_exfremo_intercept = 0.0 _d 0
      climsst_exfremo_slope = 0.0 _d 0

      climsssstartdate1  = 0
      climsssstartdate2  = 0
      climsssperiod      = 0
      climsssconst         = 0.0 _d 0
      climsss_exfremo_intercept = 0.0 _d 0
      climsss_exfremo_slope = 0.0 _d 0

c     Data files.
      climsstfile        = ' '
      climsssfile        = ' '

c     Input scaling factors.
      exf_inscal_climsst     =  1. _d 0
      exf_inscal_climsss     =  1. _d 0

c     Start dates.
      climsststartdate   = 0.
      climsssstartdate   = 0.

c     Initialise freezing temperature of sea water
      climtempfreeze = -1.9 _d 0

c     Initialise file type and field precision
      exf_clim_iprec  = 32
      exf_clim_yftype = 'RL'

#ifdef USE_EXF_INTERPOLATION
      climsst_lon0    = thetaMin + delX(1) / 2
      climsss_lon0    = thetaMin + delX(1) / 2
      climsst_lat0    = phimin   + delY(1) / 2
      climsss_lat0    = phimin   + delY(1) / 2
      climsst_nlon    = Nx
      climsst_nlat    = Ny
      climsss_nlon    = Nx
      climsss_nlat    = Ny
      climsst_lon_inc = delX(1)
      climsss_lon_inc = delX(1)
      DO i=1,MAX_LAT_INC
         IF (i.LT.Ny) THEN
            climsst_lat_inc(i)   = (delY(i) + delY(i)) / 2.
            climsss_lat_inc(i)   = (delY(i) + delY(i)) / 2.
         ELSE
            climsst_lat_inc(i)   = 0.
            climsss_lat_inc(i)   = 0.
         ENDIF
      ENDDO
#endif /* USE_EXF_INTERPOLATION */

c     Next, read the forcing data file.
        WRITE(msgBuf,'(A)') 'EXF_CLIM_READPARMS: opening data.exf_clim'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

        CALL OPEN_COPY_DATA_FILE(
     I                          'data.exf_clim', 'EXF_CLIM_READPARMS',
     O                          iUnit,
     I                          myThid )

      READ(  iUnit, nml = exf_clim_nml )

      WRITE(msgBuf,'(A)') 
     &     'EXF_CLIM_READPARMS: finished reading data.exf_clim'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                SQUEEZE_RIGHT , 1)

      CLOSE( iUnit )

c     Complete the start date specifications for the forcing
c     fields to get a complete calendar date array.

c     check for consistency

      if (.NOT. (exf_clim_iprec .EQ. 32
     &      .OR. exf_clim_iprec .EQ. 64)) then
      stop 'stopped in exf_clim_readparms: value of iprec not allowed'
      else if
     &   (.NOT. (exf_clim_yftype .EQ. 'RS'
     &      .OR. exf_clim_yftype .EQ. 'RL'))
     &          then
      stop 'stopped in exf_clim_readparms: value of yftype not allowed'
      end if           

#ifdef USE_EXF_INTERPOLATION
      if ( climsst_nlat .GT. MAX_LAT_INC )
     & stop 'stopped in exf_clim_readparms: climsst_nlat > MAX_LAT_INC'
      if ( climsss_nlat .GT. MAX_LAT_INC )
     & stop 'stopped in exf_clim_readparms: climsss_nlat > MAX_LAT_INC'
#endif


#ifdef ALLOW_CLIMTEMP_RELAXATION
      if ( climtempfile .NE. ' ' .AND. climtempperiod .NE. 0. ) then
         call cal_FullDate( climtempstartdate1, climtempstartdate2,
     &        date_array,                    mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds (difftime,      climtempstartdate  ,mythid)
         climtempstartdate=modelstart+climtempstartdate
      endif
#endif

#ifdef ALLOW_CLIMSALT_RELAXATION
      if ( climsaltfile .NE. ' ' .AND. climsaltperiod .NE. 0. ) then      
        call cal_FullDate( climsaltstartdate1, climsaltstartdate2,
     &                     date_array,                    mythid )
        call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
        call cal_ToSeconds (difftime,      climsaltstartdate  ,mythid)
        climsaltstartdate=modelstart+climsaltstartdate
      endif
#endif

#ifdef ALLOW_CLIMSST_RELAXATION
      if ( climsstfile .NE. ' ' .AND. climsstperiod .NE. 0. ) then
         call cal_FullDate( climsststartdate1, climsststartdate2,
     &        date_array,                  mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds (difftime,       climsststartdate  ,mythid)
         climsststartdate=modelstart+climsststartdate
      endif
#endif

#ifdef ALLOW_CLIMSSS_RELAXATION
      if ( climsssfile .NE. ' ' .AND. climsssperiod .NE. 0. ) then
         call cal_FullDate( climsssstartdate1, climsssstartdate2,
     &        date_array,                  mythid )
         call cal_TimePassed(modelstartdate,date_array,difftime,mythid)
         call cal_ToSeconds (difftime,       climsssstartdate  ,mythid)
         climsssstartdate=modelstart+climsssstartdate
      endif
#endif

      _END_MASTER( mythid )

      _BARRIER

      end
