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
#include "CTRL_SIZE.h"
#include "CTRL_GENARR.h"

C     === Routine arguments ===
C     myThid     :: my Thread Id number
      INTEGER myThid

C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL solve4Stress

#ifndef ALLOW_OPENAD
#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
      integer iarr, k2
#endif
#endif

      WRITE(msgBuf,'(A)') 'CTRL_CHECK: #define ALLOW_CTRL'
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

#if ((!defined ALLOW_GENARR2D_CONTROL) && (!defined ALLOW_GENARR3D_CONTROL) \
      && (!defined ALLOW_GENTIM2D_CONTROL))
      if (ctrlUseGen) then
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK : ',
     &     'Cannot ctrlUseGen unless ALLOW_GEN*_CONTROL is defined '
      CALL PRINT_ERROR( msgBuf, myThid )
      STOP 'ABNORMAL END: S/R CTRL_CHECK'
      endif
#endif

#ifdef ALLOW_GENTIM2D_CONTROL
      IF ( ctrlUseGen .AND. .NOT.useCAL ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CTRL_CHECK: ',
     &    'ALLOW_GENTIM2D_CONTROL and ctrlUseGen=T is'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** ',
     &    ' unsafe without calendar package (currently: useCAL=F)'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
#endif

#ifndef ALLOW_OPENAD

#ifdef ALLOW_GENARR2D_CONTROL
        do iarr = 1, maxCtrlArr2D
          if (((xx_genarr2d_weight(iarr).NE.' ').AND.
     &         (xx_genarr2d_file(iarr).EQ.' ')).OR.
     &        ((xx_genarr2d_weight(iarr).EQ.' ').AND.
     &         (xx_genarr2d_file(iarr).NE.' '))) then
           WRITE(msgBuf,'(3A)') 'CTRL_CHECK : ',
     &      'Cannot define xx_genarr2d_weight or xx_genarr2d_file ',
     &      'separately'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R CTRL_CHECK'
          endif
        enddo
#endif

#ifdef ALLOW_GENARR3D_CONTROL
        do iarr = 1, maxCtrlArr3D
          if (((xx_genarr3d_weight(iarr).NE.' ').AND.
     &         (xx_genarr3d_file(iarr).EQ.' ')).OR.
     &        ((xx_genarr3d_weight(iarr).EQ.' ').AND.
     &         (xx_genarr3d_file(iarr).NE.' '))) then
           WRITE(msgBuf,'(3A)') 'CTRL_CHECK : ',
     &      'Cannot define xx_genarr3d_weight or xx_genarr3d_file ',
     &      'separately'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R CTRL_CHECK'
          endif
        enddo
#endif

#ifdef ALLOW_GENTIM2D_CONTROL
        do iarr = 1, maxCtrlTim2D
          if (((xx_gentim2d_weight(iarr).NE.' ').AND.
     &         (xx_gentim2d_file(iarr).EQ.' ')).OR.
     &        ((xx_gentim2d_weight(iarr).EQ.' ').AND.
     &         (xx_gentim2d_file(iarr).NE.' '))) then
           WRITE(msgBuf,'(3A)') 'CTRL_CHECK : ',
     &      'Cannot define xx_gentim2d_weight or xx_gentim2d_file ',
     &      'separately'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R CTRL_CHECK'
          endif
        enddo
#endif

#endif /* ALLOW_OPENAD */

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

#ifdef ALLOW_GENARR2D_CONTROL
c       Warn user: doscaling must be true to do log10 ctrl
        do iarr = 1, maxCtrlArr2D
          do k2 = 1, maxCtrlProc
            if ( (xx_genarr2d_preproc(k2,iarr).eq.'noscaling') .and.
     &         (xx_genarr2d_preproc_c(k2,iarr).eq.'log10ctrl') ) then
              write(msgBuf,'(2A)')
     &        'CTRL_CHECK: conflicting ',
     &        'preproc_c & preproc flags '
              call print_error( msgBuf, myThid )
              write(msgBuf,'(A,I2,A)')
     &        'log10ctrl and noscaling set for xx_genarr2d(',iarr,
     &        '). '
              call print_error( msgBuf, myThid )
              STOP 'ABNORMAL END: S/R CTRL_CHECK'
            endif
          enddo
        enddo
#endif

#ifdef ALLOW_GENARR3D_CONTROL
c       Warn user: doscaling must be true to do log10 ctrl
        do iarr = 1, maxCtrlArr3D
          do k2 = 1, maxCtrlProc
            if ( (xx_genarr3d_preproc(k2,iarr).eq.'noscaling') .and.
     &         (xx_genarr3d_preproc_c(k2,iarr).eq.'log10ctrl') ) then
              write(msgBuf,'(2A)')
     &        'CTRL_CHECK: conflicting ',
     &        'preproc_c & preproc flags '
              call print_error( msgBuf, myThid )
              write(msgBuf,'(A,I2,A)')
     &        'log10ctrl and noscaling set for xx_genarr2d(',iarr,
     &        '). '
              call print_error( msgBuf, myThid )
              STOP 'ABNORMAL END: S/R CTRL_CHECK'
            endif
          enddo
        enddo
#endif

#ifdef ALLOW_SHI2D_CONTROL
# ifndef ALLOW_GENARR2D_CONTROL
         WRITE(msgBuf,'(3A)')
     &        'ALLOW_SHI2D_CONTROL: used for 2D control var ',
     &        'of 2D wet points with pkg/shelfice only works with ',
     &        'ALLOW_GENARR2D_CONTROL'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R CTRL_CHECK'
# endif
#endif

      RETURN
      END
