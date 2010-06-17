C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_check.F,v 1.14 2010/06/17 21:45:16 gforget Exp $
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

C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

c     == end of interface ==

c     check for consistency
      if (.NOT.
     &     (exf_iprec.EQ.precFloat32 .OR. exf_iprec.EQ.precFloat64)
     &     ) then
       stop 'S/R EXF_CHECK: value of exf_iprec not allowed'
      elseif ( exf_yftype.NE.'RL' ) then
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

#ifndef ALLOW_ZENITHANGLE
      IF ( useExfZenAlbedo .OR. useExfZenIncoming .OR. 
     &     select_ZenAlbedo .NE. 0 ) THEN
        WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: unsupported option',
     &        ' when ALLOW_ZENITHANGLE is not defined'
        CALL PRINT_ERROR( msgBuf , mythid)
        STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF
#endif

#ifdef ALLOW_ZENITHANGLE
      IF ( usingCartesianGrid .OR. usingCylindricalGrid ) then
        WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: ALLOW_ZENITHANGLE does ',
     &        'not work for carthesian and cylindrical grids'
        CALL PRINT_ERROR( msgBuf , mythid)
        STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF
      IF ( select_ZenAlbedo.LT.0 .OR. select_ZenAlbedo.GT.3 ) then
        WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: unsupported ',
     &        'select_ZenAlbedo choice'
        CALL PRINT_ERROR( msgBuf , mythid)
        STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF
      IF ( select_ZenAlbedo.EQ.2 .) then
        write(standardmessageunit,'(A,A)') 
     &  'EXF WARNING: for daily mean albedo, it is advised ',
     &        'to use select_ZenAlbedo.EQ.1 instead of 2'
      ENDIF
      IF ( select_ZenAlbedo.GT.2 .AND. swdownperiod.GT. 21600 ) then
       WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: using diurnal albedo ',
     &        'formula requires diurnal downward shortwave forcing'
        CALL PRINT_ERROR( msgBuf , mythid)
        STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF
      IF ( select_ZenAlbedo.GT.2 .AND. swdownperiod.GT. 3600 ) then
        write(standardmessageunit,'(A,A)')  
     &  'EXF WARNING: the diurnal albedo formula is likely not safe ',
     &  'for such coarse temporal resolution downward shortwave forcing'
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
