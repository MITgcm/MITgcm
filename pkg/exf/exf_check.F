C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_check.F,v 1.22 2014/01/06 14:52:38 mlosch Exp $
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
      IF (.NOT.
     &     (exf_iprec.EQ.precFloat32 .OR. exf_iprec.EQ.precFloat64)
     &     ) THEN
       WRITE(msgBuf,'(A)')
     &      'S/R EXF_CHECK: value of exf_iprec not allowed'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF

      IF (repeatPeriod.lt.0.) THEN
       WRITE(msgBuf,'(A)') 
     &      'S/R EXF_CHECK: repeatPeriod must be positive'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF

      IF (useExfYearlyFields.and.repeatPeriod.ne.0.) THEN
       WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: The use of ',
     $      'useExfYearlyFields AND repeatPeriod is not implemented'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF

#ifdef ALLOW_BULKFORMULAE
      IF ( useAtmWind ) THEN
      IF ( ustressfile .NE. ' ' .OR. vstressfile .NE. ' ' ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R EXF_CHECK: use u,v_wind components but not wind-stress'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF
      ENDIF
#endif

      IF ( .NOT.useAtmWind ) THEN
       IF ( uwindfile .NE. ' ' .OR. vwindfile .NE. ' ' ) THEN
        WRITE(msgBuf,'(A)')
     & 'S/R EXF_CHECK: read-in wind-stress but not u,v_wind components'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
       ENDIF
      ENDIF

#ifndef ALLOW_ZENITHANGLE
      IF ( useExfZenAlbedo .OR. useExfZenIncoming .OR.
     &     select_ZenAlbedo .NE. 0 ) THEN
        WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: unsupported option',
     &        ' when ALLOW_ZENITHANGLE is not defined'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF
#endif

#ifdef ALLOW_ZENITHANGLE
      IF ( usingCartesianGrid .OR. usingCylindricalGrid ) THEN
       WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: ALLOW_ZENITHANGLE does ',
     &      'not work for carthesian and cylindrical grids'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF
      IF ( select_ZenAlbedo.LT.0 .OR. select_ZenAlbedo.GT.3 ) THEN
       WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: unsupported ',
     &      'select_ZenAlbedo choice'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF
      IF ( select_ZenAlbedo.EQ.2 .) THEN
       WRITE(msgBuf,'(A,A)') 
     &      'S/R EXF_CHECK: *** WARNING *** for daily mean albedo, ',
     &      'it is advised to use select_ZenAlbedo.EQ.1 instead of 2'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
      IF ( select_ZenAlbedo.GT.2 .AND. swdownperiod.GT. 21600 ) THEN
       WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: using diurnal albedo ',
     &        'formula requires diurnal downward shortwave forcing'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF
      IF ( select_ZenAlbedo.GT.2 .AND. swdownperiod.GT. 3600 ) then
       WRITE(msgBuf,'(A,A,A)')
     &      'S/R EXF_CHECK: *** WARNING *** ',
     &      'the diurnal albedo formula is likely not safe for such ',
     &      'coarse temporal resolution downward shortwave forcing'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
#endif

#ifdef USE_EXF_INTERPOLATION
      IF ( climsstfile .NE. ' ' ) THEN
       IF ( climsst_nlat .GT. MAX_LAT_INC ) THEN
        WRITE(msgBuf,'(A)') 'S/R EXF_CHECK: climsst_nlat > MAX_LAT_INC'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
       ENDIF
      ENDIF
      IF ( climsssfile .NE. ' ' ) THEN
       IF ( climsss_nlat .GT. MAX_LAT_INC ) THEN
        WRITE(msgBuf,'(A)') 'S/R EXF_CHECK: climsss_nlat > MAX_LAT_INC'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
       ENDIF
      ENDIF
      IF ( usingCartesianGrid ) THEN
       WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: ',
     &      'USE_EXF_INTERPOLATION assumes latitude/longitude'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: ',
     &      'input and output coordinates. Trivial to extend to'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: ',
     &      'cartesian coordinates, but has not yet been done.'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF
C-    some restrictions on 2-component vector field (might be relaxed later on)
      IF ( ( uwind_interpMethod.GE.1 .AND. uwindfile.NE.' ' ) .OR.
     &     ( vwind_interpMethod.GE.1 .AND. vwindfile.NE.' ' ) ) THEN
       IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
        IF ( uwind_interpMethod.EQ.0 .OR. uwindfile.EQ.' ' .OR.
     &       vwind_interpMethod.EQ.0 .OR. vwindfile.EQ.' ' ) THEN
C-    stop if one expects interp+rotation (Curvilin-G) which will not happen
         WRITE(msgBuf,'(A)') 
     &        'S/R EXF_CHECK: interp. needs 2 components (wind)'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R EXF_CHECK'
        ENDIF
        IF ( uwindstartdate .NE. vwindstartdate .OR.
     &       uwindperiod    .NE. vwindperiod   ) THEN
         WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: ',
     &        'For CurvilinearGrid/RotatedGrid, the u and v wind '
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,A,A)') 'S/R EXF_CHECK: ',
     &        'files have to have the same startdate and period, ',
     &        'because S/R EXF_SET_UV assumes that.'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R EXF_CHECK'
        ENDIF
       ENDIF
      ENDIF
      IF ( (ustress_interpMethod.GE.1 .AND. ustressfile.NE.' ') .OR.
     &     (vstress_interpMethod.GE.1 .AND. vstressfile.NE.' ') ) THEN
       IF ( readStressOnCgrid ) THEN
        WRITE(msgBuf,'(A,A)') 
     &       'S/R EXF_CHECK: readStressOnCgrid=.TRUE. ',
     &       'and interp wind-stress (=A-grid) are not compatible'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
       ENDIF
       IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
        IF ( ustress_interpMethod.EQ.0 .OR. ustressfile.EQ.' ' .OR.
     &       vstress_interpMethod.EQ.0 .OR. vstressfile.EQ.' ' ) THEN
C-    stop if one expects interp+rotation (Curvilin-G) which will not happen
         WRITE(msgBuf,'(A)') 
     &        'S/R EXF_CHECK: interp. needs 2 components (wind-stress)'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R EXF_CHECK'
        ENDIF
        IF ( ustressstartdate .NE. vstressstartdate .OR.
     &       ustressperiod    .NE. vstressperiod   ) THEN
         WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: ',
     &    'For CurvilinearGrid/RotatedGrid, the u and v wind stress '
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,A,A)') 'S/R EXF_CHECK: ',
     &        'files have to have the same startdate and period, ',
     &        'because S/R EXF_SET_UV assumes that.'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R EXF_CHECK'
        ENDIF
       ENDIF
      ENDIF

      IF ( (ustress_interpMethod.EQ.0 .AND. ustressfile.NE.' ') .OR.
     &     (vstress_interpMethod.EQ.0 .AND. vstressfile.NE.' ') ) THEN
#else /* ifndef USE_EXF_INTERPOLATION */
      IF     ( ustressfile .NE. ' ' .OR. vstressfile .NE. ' ' ) THEN
#endif /* USE_EXF_INTERPOLATION */
       IF (     (readStressOnAgrid.AND.readStressOnCgrid) .OR.
     &      .NOT.(readStressOnAgrid.OR.readStressOnCgrid) ) THEN
        WRITE(msgBuf,'(A)')
     & 'S/R EXF_CHECK: Select 1 wind-stress position: A or C-grid'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
       ENDIF
      ELSE
       IF ( readStressOnAgrid .OR. readStressOnCgrid ) THEN
        WRITE(msgBuf,'(A)')
     &       'S/R EXF_CHECK: wind-stress position irrelevant'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
       ENDIF
      ENDIF

#ifdef USE_NO_INTERP_RUNOFF
      WRITE(msgBuf,'(A)') 
     &     'S/R EXF_CHECK: USE_NO_INTERP_RUNOFF code has been removed;'
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(A,A)') 
     &     'S/R EXF_CHECK: use instead "runoff_interpMethod=0"',
     &      ' in "data.exf" (EXF_NML_04)'
      CALL PRINT_ERROR( msgBuf, myThid )
      STOP 'ABNORMAL END: S/R EXF_CHECK'
#endif /* USE_NO_INTERP_RUNOFF */

#ifdef ALLOW_CLIMTEMP_RELAXATION
      WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: ',
     &     'ALLOW_CLIMTEMP_RELAXATION no longer supported. Use pkg/rbcs'
      CALL PRINT_ERROR( msgBuf, myThid )
      STOP 'ABNORMAL END: S/R EXF_CHECK'
#endif

#ifdef ALLOW_CLIMSALT_RELAXATION
      WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: ',
     &     'ALLOW_CLIMSALT_RELAXATION no longer supported. Use pkg/rbcs'
      CALL PRINT_ERROR( msgBuf, myThid )
      STOP 'ABNORMAL END: S/R EXF_CHECK'
#endif

      IF ( climsstTauRelax.NE.0. ) THEN
#ifndef ALLOW_CLIMSST_RELAXATION
       WRITE(msgBuf,'(A)') 'S/R EXF_CHECK: climsstTauRelax > 0'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 
     &      'S/R EXF_CHECK: but ALLOW_CLIMSST_RELAXATION is not defined'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R EXF_CHECK'
#endif
       IF ( climsstfile.EQ.' ' ) THEN
        WRITE(msgBuf,'(A)') 'S/R EXF_CHECK: climsstTauRelax > 0 but'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'S/R EXF_CHECK: climsstfile is not set'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
       ENDIf
      ENDIf
      
      IF ( climsssTauRelax.NE.0. ) THEN
#ifndef ALLOW_CLIMSSS_RELAXATION
       WRITE(msgBuf,'(A)') 'S/R EXF_CHECK: climsssTauRelax > 0'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 
     &      'S/R EXF_CHECK: but ALLOW_CLIMSSS_RELAXATION is not defined'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R EXF_CHECK'
#endif
       IF ( climsssfile.EQ.' ' ) THEN
        WRITE(msgBuf,'(A)') 'S/R EXF_CHECK: climsssTauRelax > 0 but'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'S/R EXF_CHECK: climsssfile is not set'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
       ENDIF
      ENDIF

      RETURN
      END
