C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_check.F,v 1.17 2014/09/19 20:38:39 gforget Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#include "AD_CONFIG.h"

      SUBROUTINE CTRL_CHECK( myThid )
C     *==========================================================*
C     | SUBROUTINE CTRK_CHECK
C     | o Validate basic package setup and inter-package
C     | dependencies.
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ctrl.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif

C     === Routine arguments ===
C     myThid -  Number of this instance
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL solve4Stress

      WRITE(msgBuf,'(A)') 'CTRL_CHECK: ctrl package'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C     Can not have both atmos. state and flux field as control

#ifdef ALLOW_TANGENTLINEAR_RUN
      if ( yadmark .NE. 'g_' ) then
         WRITE(msgBuf,'(A)')
     &        'yadmark very likely wrong in data.ctrl for case '
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &        'ALLOW_TANGENTLINEAR_RUN; should be g_ '
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R CTRL_CHECK'
      endif
#endif

#if (!defined (ALLOW_GENARR2D_CONTROL) && !defined (ALLOW_GENARR3D_CONTROL) && !defined (ALLOW_GENTIM2D_CONTROL))
      if (ctrlUseGen) then
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK : ',
     &     'Cannot ctrlUseGen unless ALLOW_GEN*_CONTROL is defined '
      CALL PRINT_ERROR( msgBuf, myThid )
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
      endif
#endif

#ifdef ALLOW_DIFFKR_CONTROL
C-    to use DIFFKR_CONTROL, needs to define ALLOW_3D_DIFFKR in CPP_OPTIONS.h
#ifndef ALLOW_3D_DIFFKR
        WRITE(msgBuf,'(A)')
     &        'Needs to define ALLOW_3D_DIFFKR to use DIFFKR_CONTROL'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif
#endif /* ALLOW_DIFFKR_CONTROL */

#if (defined (ALLOW_HFLUX_CONTROL) && defined (ALLOW_ATEMP_CONTROL))
      WRITE(msgBuf,'(A)')
     &     'Cannot have both ALLOW_HFLUX_CONTROL & ALLOW_ATEMP_CONTROL'
      CALL PRINT_ERROR( msgBuf, myThid )
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif

#if (defined (ALLOW_SFLUX_CONTROL) && defined (ALLOW_AQH_CONTROL))
      WRITE(msgBuf,'(A)')
     &     'Cannot have both ALLOW_SFLUX_CONTROL & ALLOW_AQH_CONTROL'
      CALL PRINT_ERROR( msgBuf, myThid )
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif

#ifdef  ALLOW_ATEMP_CONTROL
#ifndef ALLOW_ATM_TEMP
      WRITE(msgBuf,'(A)')
     &     'ALLOW_ATEMP_CONTROL used without ALLOW_ATM_TEMP'
      CALL PRINT_ERROR( msgBuf, myThid )
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif
#endif

#ifdef  ALLOW_AQH_CONTROL
#ifndef ALLOW_ATM_TEMP
      WRITE(msgBuf,'(A)')
     &     'ALLOW_AQH_CONTROL used without ALLOW_ATM_TEMP'
      CALL PRINT_ERROR( msgBuf, myThid )
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif
#endif

#if ( defined ALLOW_UWIND_CONTROL || defined ALLOW_VWIND_CONTROL )
#ifdef ALLOW_EXF
      IF ( .NOT.useEXF .OR. .NOT.useAtmWind ) THEN 
#endif
      WRITE(msgBuf,'(3A)') 'CTRL_CHECK : ',
     &     'ALLOW_U / VWIND_CONTROL used without exf & ',
     &     'useAtmWind will have no effect '      
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &  SQUEEZE_RIGHT, myThid )
#ifdef ALLOW_EXF
      ENDIF
#endif
#endif

#if ( defined ALLOW_USTRESS_CONTROL || defined ALLOW_VSTRESS_CONTROL )
#ifdef ALLOW_EXF
      IF ( useAtmWind ) THEN
      WRITE(msgBuf,'(3A)') 'CTRL_CHECK : ',
     &     'ALLOW_U / VSTRESS_CONTROL used with exf & ',
     &     'useAtmWind will have no effect '
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &  SQUEEZE_RIGHT, myThid )
      ENDIF
#endif
#endif

#ifdef ALLOW_SEAICE
C     pkg/seaice requires a particular set of control parameters
C     which in turn require a particular set of input fields
# if ( defined ALLOW_HFLUX_CONTROL || defined ALLOW_SFLUX_CONTROL )
      WRITE(msgBuf,'(A)')
     &     'The SEAICE adjoint does not allow the surface fluxes'
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(A)') '  ALLOW_HFLUX_CONTROL'
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(A)') '  ALLOW_SFLUX_CONTROL'
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(A)') 'to be control variables'
      CALL PRINT_ERROR( msgBuf, myThid )
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
# endif
# if ( defined ALLOW_USTRESS_CONTROL || defined ALLOW_VSTRESS_CONTROL )
      if ( useAtmWind ) then
        solve4Stress = .TRUE.
      else
#  ifdef ALLOW_BULK_LARGEYEAGER04
      solve4Stress = wspeedfile .NE. ' '
#  else
      solve4Stress = .FALSE.
#  endif
      endif
      if (.NOT.solve4Stress) then
        WRITE(msgBuf,'(A)')
     &     'The SEAICE adjoint does not allow the surface fluxes'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') '  ALLOW_USTRESS_CONTROL'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') '  ALLOW_VSTRESS_CONTROL'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'to be control variables'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'except if wspeedfile is specified'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'and ifdef ALLOW_BULK_LARGEYEAGER04'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CTRL_CHECK'
      endif
# endif
#endif /* ALLOW_SEAICE */

#ifndef ALLOW_PACKUNPACK_METHOD2
# ifndef EXCLUDE_CTRL_PACK
       IF ( useSingleCpuIO ) THEN
        WRITE(msgBuf,'(3A)') '** WARNING ** CTRL_CHECK: ',
     &        'relying on mdsio_gl.F to pack/unpack the control',
     &        'vector is unsafe when useSingleCpuIO is true.'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit, 
     &  SQUEEZE_RIGHT, myThid )
       ENDIF
# endif
#endif

#ifdef ALLOW_PACKUNPACK_METHOD2
#ifndef EXCLUDE_CTRL_PACK
#if (defined (ALLOW_OBCSN_CONTROL) || \
     defined (ALLOW_OBCSS_CONTROL) || \
     defined (ALLOW_OBCSW_CONTROL) || \
     defined (ALLOW_OBCSE_CONTROL))
        WRITE(msgBuf,'(2A)') 
     &        'ALLOW_PACKUNPACK_METHOD2 does not work with',
     &        'open boundary controls (see verif/obcs_ctrl).'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R CTRL_CHECK'
#endif
#endif
#endif

      RETURN
      END
