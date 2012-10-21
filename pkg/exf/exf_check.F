C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_check.F,v 1.19 2012/10/21 20:06:00 gforget Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      SUBROUTINE EXF_CHECK( myThid )

c     ==================================================================
c     SUBROUTINE EXF_CHECK
c     ==================================================================
c
      IMPLICIT NONE

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"
c     == routine arguments ==

c     myThid - thread number for this instance of the routine.

      INTEGER myThid

c     == local variables ==

C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

c     == end of interface ==

c     check for consistency
      if (.NOT.
     &     (exf_iprec.EQ.precFloat32 .OR. exf_iprec.EQ.precFloat64)
     &     ) then
       stop 'S/R EXF_CHECK: value of exf_iprec not allowed'
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
        WRITE(msgBuf,'(A,A)') 'EXF_CHECK: unsupported option',
     &        ' when ALLOW_ZENITHANGLE is not defined'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF
#endif

#ifdef ALLOW_ZENITHANGLE
      IF ( usingCartesianGrid .OR. usingCylindricalGrid ) then
        WRITE(msgBuf,'(A,A)') 'EXF_CHECK: ALLOW_ZENITHANGLE does ',
     &        'not work for carthesian and cylindrical grids'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF
      IF ( select_ZenAlbedo.LT.0 .OR. select_ZenAlbedo.GT.3 ) then
        WRITE(msgBuf,'(A,A)') 'EXF_CHECK: unsupported ',
     &        'select_ZenAlbedo choice'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF
      IF ( select_ZenAlbedo.EQ.2 .) then
        write(standardmessageunit,'(A,A)')
     &  'EXF WARNING: for daily mean albedo, it is advised ',
     &        'to use select_ZenAlbedo.EQ.1 instead of 2'
      ENDIF
      IF ( select_ZenAlbedo.GT.2 .AND. swdownperiod.GT. 21600 ) then
       WRITE(msgBuf,'(A,A)') 'EXF_CHECK: using diurnal albedo ',
     &        'formula requires diurnal downward shortwave forcing'
        CALL PRINT_ERROR( msgBuf, myThid )
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
C-    some restrictions on 2-component vector field (might be relaxed later on)
      IF ( ( uwind_interpMethod.GE.1 .AND. uwindfile.NE.' ' ) .OR.
     &     ( vwind_interpMethod.GE.1 .AND. vwindfile.NE.' ' ) ) THEN
       IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         IF ( uwind_interpMethod.EQ.0 .OR. uwindfile.EQ.' ' .OR.
     &        vwind_interpMethod.EQ.0 .OR. vwindfile.EQ.' ' ) THEN
C-    stop if one expects interp+rotation (Curvilin-G) which will not happen
          STOP 'interp. needs 2 components (wind)'
         ENDIF
         IF ( uwindstartdate .NE. vwindstartdate .OR.
     &        uwindperiod    .NE. vwindperiod   ) THEN
          print*,'For CurvilinearGrid/RotatedGrid, S/R EXF_SET_UV'
          print*,'assumes that the u and v wind files'
          print*,'have the same startdate and period.'
          stop
         ENDIF
       ENDIF
      ENDIF
      IF ( (ustress_interpMethod.GE.1 .AND. ustressfile.NE.' ') .OR.
     &     (vstress_interpMethod.GE.1 .AND. vstressfile.NE.' ') ) THEN
       IF ( readStressOnCgrid ) THEN
          STOP 'readStressOnCgrid and interp wind-stress (=A-grid)'
       ENDIF
       IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
         IF ( ustress_interpMethod.EQ.0 .OR. ustressfile.EQ.' ' .OR.
     &        vstress_interpMethod.EQ.0 .OR. vstressfile.EQ.' ' ) THEN
C-    stop if one expects interp+rotation (Curvilin-G) which will not happen
          STOP 'interp. needs 2 components (wind-stress)'
         ENDIF
         IF ( ustressstartdate .NE. vstressstartdate .OR.
     &        ustressperiod    .NE. vstressperiod   ) THEN
          print*,'For CurvilinearGrid/RotatedGrid, S/R EXF_SET_UV'
          print*,'assumes that the u and v wind stress files'
          print*,'have the same startdate and period.'
          stop
         ENDIF
       ENDIF
      ENDIF

      IF ( (ustress_interpMethod.EQ.0 .AND. ustressfile.NE.' ') .OR.
     &     (vstress_interpMethod.EQ.0 .AND. vstressfile.NE.' ') ) THEN
#else /* ifndef USE_EXF_INTERPOLATION */
      IF     ( ustressfile .NE. ' ' .OR. vstressfile .NE. ' ' ) THEN
#endif /* USE_EXF_INTERPOLATION */
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

#ifdef USE_NO_INTERP_RUNOFF
      WRITE(msgBuf,'(A,A)') 'EXF_CHECK: USE_NO_INTERP_RUNOFF code',
     &        ' has been removed;'
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(A,A)') 'use instead "runoff_interpMethod=0"',
     &      ' in "data.exf" (EXF_NML_04)'
      CALL PRINT_ERROR( msgBuf, myThid )
      STOP 'ABNORMAL END: S/R EXF_CHECK'
#endif /* USE_NO_INTERP_RUNOFF */

#ifdef ALLOW_CLIMTEMP_RELAXATION
      STOP 'ALLOW_CLIMTEMP_RELAXATION no longer supported. Use pkg/rbcs'
#endif

#ifdef ALLOW_CLIMSALT_RELAXATION
      STOP 'ALLOW_CLIMSALT_RELAXATION no longer supported. Use pkg/rbcs'
#endif

      IF ( climsstTauRelax.NE.0. ) THEN
#ifndef ALLOW_CLIMSST_RELAXATION
        WRITE(msgBuf,'(A)') 'EXF_CHECK: climsstTauRelax > 0'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &        'but ALLOW_CLIMSST_RELAXATION is not defined'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
#endif
       IF ( climsstfile.EQ.' ' ) THEN
        WRITE(msgBuf,'(A)')
     &     'S/R EXF_CHECK: climsstTauRelax > 0 but'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &     'S/R EXF_CHECK: climsstfile is not set'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
       ENDIf
      ENDIf

      IF ( climsssTauRelax.NE.0. ) THEN
#ifndef ALLOW_CLIMSSS_RELAXATION
        WRITE(msgBuf,'(A)') 'EXF_CHECK: climsssTauRelax > 0'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &        'but ALLOW_CLIMSSS_RELAXATION is not defined'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
#endif
       IF ( climsssfile.EQ.' ' ) THEN
        WRITE(msgBuf,'(A)')
     &     'S/R EXF_CHECK: climsssTauRelax > 0 but'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &     'S/R EXF_CHECK: climsssfile is not set'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
       ENDIF
      ENDIF

      RETURN
      END
