C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_check.F,v 1.25 2017/01/06 21:54:56 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

CBOP
C     !ROUTINE: EXF_CHECK
C     !INTERFACE:

      SUBROUTINE EXF_CHECK( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R EXF_CHECK
C     | o Check parameters and other package dependences
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid   :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount
CEOP

      _BEGIN_MASTER(myThid)
      errCount = 0

      WRITE(msgBuf,'(A)') 'EXF_CHECK: #define ALLOW_EXF'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     check for consistency
      IF (.NOT.
     &     (exf_iprec.EQ.precFloat32 .OR. exf_iprec.EQ.precFloat64)
     &     ) THEN
       WRITE(msgBuf,'(A)')
     &      'S/R EXF_CHECK: value of exf_iprec not allowed'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

      IF (repeatPeriod.lt.0.) THEN
       WRITE(msgBuf,'(A)')
     &      'S/R EXF_CHECK: repeatPeriod must be positive'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

      IF (useExfYearlyFields.and.repeatPeriod.ne.0.) THEN
       WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: The use of ',
     $      'useExfYearlyFields AND repeatPeriod is not implemented'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

#ifdef ALLOW_BULKFORMULAE
      IF ( useAtmWind ) THEN
       IF ( ustressfile .NE. ' ' .OR. vstressfile .NE. ' ' ) THEN
        WRITE(msgBuf,'(A)')
     &   'S/R EXF_CHECK: use u,v_wind components but not wind-stress'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF
#endif

      IF ( .NOT.useAtmWind ) THEN
       IF ( uwindfile .NE. ' ' .OR. vwindfile .NE. ' ' ) THEN
        WRITE(msgBuf,'(A)')
     & 'S/R EXF_CHECK: read-in wind-stress but not u,v_wind components'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF

#ifndef ALLOW_ZENITHANGLE
      IF ( useExfZenAlbedo .OR. useExfZenIncoming .OR.
     &     select_ZenAlbedo .NE. 0 ) THEN
        WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: unsupported option',
     &        ' when ALLOW_ZENITHANGLE is not defined'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

#ifdef ALLOW_ZENITHANGLE
      IF ( usingCartesianGrid .OR. usingCylindricalGrid ) THEN
       WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: ALLOW_ZENITHANGLE does ',
     &      'not work for carthesian and cylindrical grids'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( select_ZenAlbedo.LT.0 .OR. select_ZenAlbedo.GT.3 ) THEN
       WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: unsupported ',
     &      'select_ZenAlbedo choice'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
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
       errCount = errCount + 1
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
       errCount = errCount + 1
      ENDIF

      IF ( climsst_interpMethod.GE.1 .AND. climsstfile.NE.' ' ) THEN
       IF ( climsst_nlat .GT. MAX_LAT_INC ) THEN
        WRITE(msgBuf,'(A)') 'S/R EXF_CHECK: climsst_nlat > MAX_LAT_INC'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( climsst_lon_inc.GT.500. ) THEN
        WRITE(msgBuf,'(2A,1PE16.8)') 'S/R EXF_CHECK: ',
     &    'Invalid value for: climsst_lon_inc =', climsst_lon_inc
c    &    'Invalid value for: snowprecip_lon_inc =', snowprecip_lon_inc
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'S/R EXF_CHECK: Fix it ',
     &    'or Turn off climsst-interp (interpMethod=0)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF
      IF ( climsss_interpMethod.GE.1 .AND. climsssfile.NE.' ' ) THEN
       IF ( climsss_nlat .GT. MAX_LAT_INC ) THEN
        WRITE(msgBuf,'(A)') 'S/R EXF_CHECK: climsss_nlat > MAX_LAT_INC'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( climsss_lon_inc.GT.500. ) THEN
        WRITE(msgBuf,'(2A,1PE16.8)') 'S/R EXF_CHECK: ',
     &    'Invalid value for: climsss_lon_inc =', climsss_lon_inc
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'S/R EXF_CHECK: Fix it ',
     &    'or Turn off climsst-interp (interpMethod=0)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
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
         errCount = errCount + 1
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
         errCount = errCount + 1
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
        errCount = errCount + 1
       ENDIF
       IF ( usingCurvilinearGrid .OR. rotateGrid ) THEN
        IF ( ustress_interpMethod.EQ.0 .OR. ustressfile.EQ.' ' .OR.
     &       vstress_interpMethod.EQ.0 .OR. vstressfile.EQ.' ' ) THEN
C-    stop if one expects interp+rotation (Curvilin-G) which will not happen
         WRITE(msgBuf,'(A)')
     &        'S/R EXF_CHECK: interp. needs 2 components (wind-stress)'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
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
         errCount = errCount + 1
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
        errCount = errCount + 1
       ENDIF
       IF (rotateStressOnAgrid.AND..NOT.readStressOnAgrid) THEN
        WRITE(msgBuf,'(2A)') 'S/R EXF_CHECK: rotateStressOnAgrid ',
     & 'only applies to cases readStressOnAgrid is true'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF

      ELSE
       IF ( readStressOnAgrid .OR. readStressOnCgrid .OR.
     &      rotateStressOnAgrid) THEN
        WRITE(msgBuf,'(A)')
     &       'S/R EXF_CHECK: wind-stress position irrelevant'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
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
      errCount = errCount + 1
#endif /* USE_NO_INTERP_RUNOFF */

#ifdef ALLOW_CLIMTEMP_RELAXATION
      WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: ',
     &     'ALLOW_CLIMTEMP_RELAXATION no longer supported. Use pkg/rbcs'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
#endif

#ifdef ALLOW_CLIMSALT_RELAXATION
      WRITE(msgBuf,'(A,A)') 'S/R EXF_CHECK: ',
     &     'ALLOW_CLIMSALT_RELAXATION no longer supported. Use pkg/rbcs'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
#endif

      IF ( climsstTauRelax.NE.0. ) THEN
#ifndef ALLOW_CLIMSST_RELAXATION
       WRITE(msgBuf,'(A)') 'S/R EXF_CHECK: climsstTauRelax > 0'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &      'S/R EXF_CHECK: but ALLOW_CLIMSST_RELAXATION is not defined'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
#endif
       IF ( climsstfile.EQ.' ' ) THEN
        WRITE(msgBuf,'(A)') 'S/R EXF_CHECK: climsstTauRelax > 0 but'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'S/R EXF_CHECK: climsstfile is not set'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIf
      ENDIf

      IF ( climsssTauRelax.NE.0. ) THEN
#ifndef ALLOW_CLIMSSS_RELAXATION
       WRITE(msgBuf,'(A)') 'S/R EXF_CHECK: climsssTauRelax > 0'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &      'S/R EXF_CHECK: but ALLOW_CLIMSSS_RELAXATION is not defined'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
#endif
       IF ( climsssfile.EQ.' ' ) THEN
        WRITE(msgBuf,'(A)') 'S/R EXF_CHECK: climsssTauRelax > 0 but'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'S/R EXF_CHECK: climsssfile is not set'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'EXF_CHECK: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R EXF_CHECK'
      ENDIF

      _END_MASTER(myThid)

      RETURN
      END
