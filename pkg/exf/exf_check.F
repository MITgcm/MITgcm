C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_check.F,v 1.6 2007/05/14 19:34:57 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      subroutine exf_check( mythid )

c     ==================================================================
c     SUBROUTINE exf_check
c     ==================================================================
c
      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
c#include "FFIELDS.h"
c#include "GRID.h"

#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"
c#include "EXF_FIELDS.h"
c     == routine arguments ==

c     mythid - thread number for this instance of the routine.

      integer mythid

c     == local variables ==

c     == end of interface ==

c     check for consistency
      if (.NOT. 
     &     (exf_iprec .EQ. 32 .OR. exf_iprec .EQ. 64)
     &     ) then
         stop 'stop in exf_readparms: value of exf_iprec not allowed'
      else if (.NOT. 
     &        (exf_yftype .EQ. 'RS' .OR. 
     &        exf_yftype .EQ. 'RL')
     &        ) then
         stop 'stop in exf_readparms: value of exf_yftype not allowed'
      end if

      if ( useCubedSphereExchange ) then
cph         if ( uvecfile .NE. ' ' .and. vvecfile .NE. ' ' ) then
c     some restrictions that can be relaxed later on
cph            if ( uvecstartdate .ne. vvecstartdate .or.
cph     &           uvecperiod    .ne. vvecperiod ) then
cph               print*,'For useCubedSphereExchange, S/R exf_set_uv.F'
cph               print*,'assumes that the u and v wind or wind stress'
cph               print*,'files have the same startdate and period.'
cph               stop
cph            endif
cph         endif
      endif

#if ( defined (ALLOW_BULKFORMULAE) && defined (ALLOW_ATM_WIND) )
      IF ( ustressfile .NE. ' ' .OR. ustressfile .NE. ' ' ) THEN
        STOP
     & 'S/R EXF_CHECK: use u,v_wind components but not wind-stress'
      ENDIF
#endif
#ifndef ALLOW_ATM_WIND
      IF ( uwindfile .NE. ' ' .OR. vwindfile .NE. ' ' ) THEN
        STOP
     & 'S/R EXF_CHECK: read-in wind-stress but not u,v_wind components'
      ENDIF
#endif

#ifdef USE_EXF_INTERPOLATION
      if ( climsst_nlat .GT. MAX_LAT_INC )
     & stop 'stopped in exf_readparms: climsst_nlat > MAX_LAT_INC'
      if ( climsss_nlat .GT. MAX_LAT_INC )
     & stop 'stopped in exf_readparms: climsss_nlat > MAX_LAT_INC'
      if ( usingCartesianGrid ) then
       print*,'USE_EXF_INTERPOLATION assumes latitude/longitude'
       print*,'input and output coordinates.  Trivial to extend to'
       print*,'cartesian coordinates, but has not yet been done.'
       stop
      endif
      IF ( readStressOnAgrid.OR.readStressOnCgrid ) THEN
        STOP 'wind-stress position defined by Long/Lat'
      ENDIF
#else
      IF ( ustressfile .NE. ' ' .OR. ustressfile .NE. ' ' ) THEN
        IF ( (readStressOnAgrid.AND.readStressOnCgrid) .OR.
     &   .NOT.(readStressOnAgrid.OR.readStressOnCgrid) ) THEN
          STOP
     & 'S/R EXF_CHECK: Select 1 wind-stress position: A or C-grid'
        ENDIF
      ELSE
        IF ( readStressOnAgrid .OR. readStressOnCgrid ) THEN
          STOP 'S/R EXF_CHECK: wind-stress position irrelevant'
        ENDIF
      ENDIF
#endif

#ifdef ALLOW_CLIMTEMP_RELAXATION
      STOP 'ALLOW_CLIMTEMP_RELAXATION no longer supported. Use pkg/rbcs'
#endif


#ifdef ALLOW_CLIMSALT_RELAXATION
      STOP 'ALLOW_CLIMSALT_RELAXATION no longer supported. Use pkg/rbcs'
#endif

      RETURN
      END
