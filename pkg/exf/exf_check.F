C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_check.F,v 1.9 2008/01/25 11:49:48 dimitri Exp $
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
       stop 'S/R EXF_CHECK: value of exf_iprec not allowed'
      elseif (.NOT. 
     &      (exf_yftype .EQ. 'RS' .OR. 
     &      exf_yftype .EQ. 'RL')
     &      ) then
       stop 'S/R EXF_CHECK: value of exf_yftype not allowed'
      endif

      if (repeatPeriod.lt.0.) then
       stop 'S/R EXF_CHECK: repeatPeriod must be positive'
      endif

      if (useExfYearlyFields.and.repeatPeriod.ne.0.) then
       print*,'Use of usefldyearlyfields AND repeatPeriod',
     &      ' not implemented'
       stop 'ABNORMAL END: S/R EXF_CHECK'
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
      if ( usingCurvilinearGrid ) then
c     some restrictions that can be relaxed later on
       if ( ustressfile .NE. ' ' .and. vstressfile .NE. ' ' ) then
        if ( ustressstartdate .ne. vstressstartdate .or.
     &       ustressperiod    .ne. vstressperiod ) then
         print*,'For useCubedSphereExchange, S/R exf_set_uv.F'
         print*,'assumes that the u and v wind stress files'
         print*,'have the same startdate and period.'
         stop
        endif
       endif
       if ( uwindfile .NE. ' ' .and. vwindfile .NE. ' ' ) then
        if ( uwindstartdate .ne. vwindstartdate .or.
     &       uwindperiod    .ne. vwindperiod ) then
         print*,'For useCubedSphereExchange, S/R exf_set_uv.F'
         print*,'assumes that the u and v wind files'
         print*,'have the same startdate and period.'
         stop
        endif
       endif
      endif
#else /* ifndef USE_EXF_INTERPOLATION */
      IF ( ustressfile .NE. ' ' .OR. vstressfile .NE. ' ' ) THEN
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
#endif /* USE_EXF_INTERPOLATION */

#ifdef ALLOW_CLIMTEMP_RELAXATION
      STOP 'ALLOW_CLIMTEMP_RELAXATION no longer supported. Use pkg/rbcs'
#endif

#ifdef ALLOW_CLIMSALT_RELAXATION
      STOP 'ALLOW_CLIMSALT_RELAXATION no longer supported. Use pkg/rbcs'
#endif

      RETURN
      END
