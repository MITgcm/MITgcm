#include "CTRL_OPTIONS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#ifdef ALLOW_SHELFICE
# include "SHELFICE_OPTIONS.h"
#endif
#include "AD_CONFIG.h"

CBOP
C     !ROUTINE: CTRL_CHECK
C     !INTERFACE:
      SUBROUTINE CTRL_CHECK( myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRK_CHECK
C     | o Validate basic ctrl package setup and inter-package
C     | dependencies.
C     *=================================================================
C     \ev

C     !USES:
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

C     !INPUT/OUTPUT PARAMETERS:
C     myThid     :: my Thread Id number
      INTEGER myThid

#if (defined (ALLOW_GENARR3D_CONTROL) && defined(ALLOW_PTRACERS))
C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
#endif

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount
#if ( defined ALLOW_USTRESS_CONTROL || defined ALLOW_VSTRESS_CONTROL )
      LOGICAL solve4Stress
#endif

#if ( defined ALLOW_GENARR2D_CONTROL || defined ALLOW_GENARR3D_CONTROL || defined ALLOW_GENTIM2D_CONTROL )
      INTEGER iarr
#endif
#if ( defined ALLOW_GENARR2D_CONTROL || defined ALLOW_GENARR3D_CONTROL )
      INTEGER k2
#endif
#if ( defined ALLOW_GENARR3D_CONTROL && defined ALLOW_PTRACERS )
      INTEGER iLen, iPtr, ascii_1
#endif
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)
      errCount = 0

      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &                     ' --> Starts to check CTRL set-up'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

#ifdef ALLOW_TANGENTLINEAR_RUN
      IF ( yadmark .NE. 'g_' ) THEN
        WRITE(msgBuf,'(A)')
     &        'yadmark very likely wrong in data.ctrl for case '
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &        'ALLOW_TANGENTLINEAR_RUN; should be g_ '
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

#if ( (!defined ALLOW_GENARR2D_CONTROL) && (!defined ALLOW_GENARR3D_CONTROL) \
      && (!defined ALLOW_GENTIM2D_CONTROL) )
      IF (ctrlUseGen) THEN
        WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &     'Cannot ctrlUseGen unless ALLOW_GEN*_CONTROL is defined '
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
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
      DO iarr = 1, maxCtrlArr2D
        IF (((xx_genarr2d_weight(iarr).NE.' ').AND.
     &         (xx_genarr2d_file(iarr).EQ.' ')).OR.
     &        ((xx_genarr2d_weight(iarr).EQ.' ').AND.
     &         (xx_genarr2d_file(iarr).NE.' '))) THEN
          WRITE(msgBuf,'(3A)') 'CTRL_CHECK: ',
     &      'Cannot define xx_genarr2d_weight or xx_genarr2d_file ',
     &      'separately'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
      ENDDO
#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
      DO iarr = 1, maxCtrlArr3D
        IF (((xx_genarr3d_weight(iarr).NE.' ').AND.
     &         (xx_genarr3d_file(iarr).EQ.' ')).OR.
     &        ((xx_genarr3d_weight(iarr).EQ.' ').AND.
     &         (xx_genarr3d_file(iarr).NE.' '))) THEN
          WRITE(msgBuf,'(3A)') 'CTRL_CHECK: ',
     &      'Cannot define xx_genarr3d_weight or xx_genarr3d_file ',
     &      'separately'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
      ENDDO
# ifdef ALLOW_PTRACERS
C     Check for valid gen-ctrl file name that mactch a ptracer index
      IF ( usePTRACERS ) THEN
        DO iarr = 1, maxCtrlArr3D
          iLen = ILNBLNK(xx_genarr3d_file(iarr))
          IF ( iLen.EQ.7 .AND.
     &         xx_genarr3d_file(iarr)(1:6).EQ.'xx_ptr' ) THEN
            ascii_1 = ICHAR( xx_genarr3d_file(iarr)(7:7) )
C     character 0 to 9 <-> 48 to 57
            IF ( ascii_1.GE.49 .AND. ascii_1.LE.57 ) THEN
              READ(xx_genarr3d_file(iarr)(7:7),*) iPtr
              WRITE(msgBuf,'(2A,I4,A)') 'CTRL_CHECK: ',
     &              'pTracer #', iPtr, ' in Gen-CTRL'
              CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                            SQUEEZE_RIGHT, myThid )
            ELSE
              WRITE(msgBuf,'(4A)') 'CTRL_CHECK: ',
     &          'Gen-CTRL file "', xx_genarr3d_file(iarr)(1:iLen), '"'
              CALL PRINT_ERROR( msgBuf, myThid )
              WRITE(msgBuf,'(2A,I3,A)') 'CTRL_CHECK:  <-- ',
     &             'do not match any pTr number (ascii=', ascii_1, ')'
              CALL PRINT_ERROR( msgBuf, myThid )
              errCount = errCount + 1
            ENDIF
          ENDIF
        ENDDO
      ENDIF
# endif /* ALLOW_PTRACERS */
#endif /* ALLOW_GENARR3D_CONTROL */

#ifdef ALLOW_GENTIM2D_CONTROL
      DO iarr = 1, maxCtrlTim2D
        IF (((xx_gentim2d_weight(iarr).NE.' ').AND.
     &         (xx_gentim2d_file(iarr).EQ.' ')).OR.
     &        ((xx_gentim2d_weight(iarr).EQ.' ').AND.
     &         (xx_gentim2d_file(iarr).NE.' '))) THEN
          WRITE(msgBuf,'(3A)') 'CTRL_CHECK: ',
     &      'Cannot define xx_gentim2d_weight or xx_gentim2d_file ',
     &      'separately'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
        ENDIF
      ENDDO
#endif /* ALLOW_GENTIM2D_CONTROL */

#endif /* ndef ALLOW_OPENAD */

#ifdef ALLOW_DIFFKR_CONTROL
C-    to use DIFFKR_CONTROL, needs to define ALLOW_3D_DIFFKR in CPP_OPTIONS.h
# ifndef ALLOW_3D_DIFFKR
      WRITE(msgBuf,'(A)')
     &        'Needs to define ALLOW_3D_DIFFKR to use DIFFKR_CONTROL'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
# endif
#endif /* ALLOW_DIFFKR_CONTROL */

#ifdef ALLOW_KAPGM_CONTROL
C-    to use KAPGM_CONTROL, needs to define GM_READ_K3D_GM in GMREDI_OPTIONS.h
# ifndef GM_READ_K3D_GM
      WRITE(msgBuf,'(A)')
     &        'Needs to define GM_READ_K3D_GM to use KAPGM_CONTROL'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
# endif
#endif /* ALLOW_KAPGM_CONTROL */

#ifdef ALLOW_KAPREDI_CONTROL
C-    to use KAPREDI_CONTROL, needs to define GM_READ_K3D_REDI in GMREDI_OPTIONS.h
# ifndef GM_READ_K3D_REDI
      WRITE(msgBuf,'(A)')
     &        'Needs to define GM_READ_K3D_REDI to use KAPREDI_CONTROL'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
# endif
#endif /* ALLOW_KAPREDI_CONTROL */

#if (defined (ALLOW_HFLUX_CONTROL) && defined (ALLOW_ATEMP_CONTROL))
      WRITE(msgBuf,'(A)')
     &     'Cannot have both ALLOW_HFLUX_CONTROL & ALLOW_ATEMP_CONTROL'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
#endif

#if (defined (ALLOW_SFLUX_CONTROL) && defined (ALLOW_AQH_CONTROL))
      WRITE(msgBuf,'(A)')
     &     'Cannot have both ALLOW_SFLUX_CONTROL & ALLOW_AQH_CONTROL'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
#endif

#ifdef  ALLOW_ATEMP_CONTROL
# ifndef ALLOW_ATM_TEMP
      WRITE(msgBuf,'(A)')
     &     'ALLOW_ATEMP_CONTROL used without ALLOW_ATM_TEMP'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
# endif
#endif /* ALLOW_ATEMP_CONTROL */

#ifdef  ALLOW_AQH_CONTROL
# ifndef ALLOW_ATM_TEMP
      WRITE(msgBuf,'(A)')
     &     'ALLOW_AQH_CONTROL used without ALLOW_ATM_TEMP'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
# endif
#endif /* ALLOW_AQH_CONTROL */

#if ( defined ALLOW_UWIND_CONTROL || defined ALLOW_VWIND_CONTROL )
# ifdef ALLOW_EXF
      IF ( .NOT.useEXF .OR. .NOT.useAtmWind ) THEN
# endif
        WRITE(msgBuf,'(3A)') '** WARNING ** CTRL_CHECK: ',
     &               'ALLOW_U/VWIND_CONTROL used without ',
     &               'exf & useAtmWind will have no effect'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
# ifdef ALLOW_EXF
      ENDIF
# endif
#endif /* ALLOW_UWIND_CONTROL or ALLOW_VWIND_CONTROL */

#if ( defined ALLOW_USTRESS_CONTROL || defined ALLOW_VSTRESS_CONTROL )
# ifdef ALLOW_EXF
      IF ( useAtmWind ) THEN
        WRITE(msgBuf,'(3A)') '** WARNING ** CTRL_CHECK: ',
     &               'ALLOW_U/VSTRESS_CONTROL used with ',
     &               'exf & useAtmWind will have no effect'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
# endif
#endif /* ALLOW_USTRESS_CONTROL or ALLOW_VSTRESS_CONTROL */

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
      errCount = errCount + 1
# endif
# if ( defined ALLOW_USTRESS_CONTROL || defined ALLOW_VSTRESS_CONTROL )
      IF ( useAtmWind ) THEN
        solve4Stress = .TRUE.
      ELSE
#  ifdef ALLOW_BULK_LARGEYEAGER04
        solve4Stress = wspeedfile .NE. ' '
#  else
        solve4Stress = .FALSE.
#  endif
      ENDIF
      IF (.NOT.solve4Stress) THEN
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
        errCount = errCount + 1
      ENDIF
# endif /* ALLOW_USTRESS_CONTROL or ALLOW_VSTRESS_CONTROL */
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
#endif /* ndef ALLOW_PACKUNPACK_METHOD2 */

#ifdef ALLOW_PACKUNPACK_METHOD2
# ifndef EXCLUDE_CTRL_PACK
#  if ( defined ALLOW_OBCSN_CONTROL || \
        defined ALLOW_OBCSS_CONTROL || \
        defined ALLOW_OBCSW_CONTROL || \
        defined ALLOW_OBCSE_CONTROL )
      WRITE(msgBuf,'(2A)')
     &        'ALLOW_PACKUNPACK_METHOD2 does not work with',
     &        'open boundary controls (see verif/obcs_ctrl).'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
#  endif
# endif /* ndef EXCLUDE_CTRL_PACK */
#endif /* ALLOW_PACKUNPACK_METHOD2 */

#ifdef ALLOW_HFLUXM_CONTROL
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &     'CPP-flag ALLOW_HFLUXM_CONTROL has been retired.'
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &     'tutorial_global_oce_optim, which was the only experiment'
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &     'that used this flag, now uses the generic control xx_qnet'
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &     'to achieve the same thing.'
      CALL PRINT_ERROR( msgBuf, myThid )
C     For now only print the error message, but do not increase the
C     error count so that the model does not stop as along as we have to
C     put up with the OpenAD version of tutorial_global_oce_optim
C      errCount = errCount + 1
#endif /* ALLOW_HFLUXM_CONTROL */

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'CTRL_CHECK: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R CTRL_CHECK'
      ENDIF

      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &                     ' <-- Ends Normally'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      _END_MASTER(myThid)

#ifdef ALLOW_GENARR2D_CONTROL
c     Warn user: doscaling must be true to do log10 ctrl
      DO iarr = 1, maxCtrlArr2D
        DO k2 = 1, maxCtrlProc
          IF ( (xx_genarr2d_preproc(k2,iarr).EQ.'noscaling') .AND.
     &       (xx_genarr2d_preproc_c(k2,iarr).EQ.'log10ctrl') ) THEN
            WRITE(msgBuf,'(A)')
     &      'CTRL_CHECK: conflicting preproc_c & preproc flags '
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A,I2,A)')
     &      'log10ctrl and noscaling set for xx_genarr2d(',iarr,').'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R CTRL_CHECK'
          ENDIF
        ENDDO
      ENDDO
# ifdef ALLOW_SHELFICE
#  ifdef SHI_ALLOW_GAMMAFRICT
C     Cannot have shiTransCoeffT/S as control variables in gammafrict form
      DO iarr = 1, maxCtrlArr2D
        IF ( (xx_genarr2d_file(iarr)(1:11).EQ.'xx_shicoeff')) THEN
          WRITE(msgBuf,'(2A,I2,A)')
     &    'shiTransCoeffT or S set as control variable for ',
     &    'xx_genarr2d(', iarr,'). '
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A)')
     &    'Cannot use shiTransCoeffT/S when SHI_ALLOW_GAMMAFRICT ',
     &    'is defined'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R CTRL_CHECK'
        ENDIF
      ENDDO
#  else
C     Cannot have shiCDrag as control variable without gammafrict form
C     this could be changed to control cdrag for momentum equation only,
C     but is probably not what the user wants or expects.
      DO iarr = 1, maxCtrlArr2D
        IF ( (xx_genarr2d_file(iarr)(1:11).EQ.'xx_shicdrag')) THEN
          WRITE(msgBuf,'(2A,I2,A)')
     &    'shiCDrag set as control variable for ',
     &    'xx_genarr2d(', iarr,'). '
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A)')
     &    'Cannot use shiCDrag when SHI_ALLOW_GAMMAFRICT ',
     &    'is NOT defined'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R CTRL_CHECK'
        ENDIF
      ENDDO
#  endif /* SHI_ALLOW_GAMMAFRICT */
# endif /* ALLOW_SHELFICE */
#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
c     Warn user: doscaling must be true to do log10 ctrl
      DO iarr = 1, maxCtrlArr3D
        DO k2 = 1, maxCtrlProc
          IF ( (xx_genarr3d_preproc(k2,iarr).EQ.'noscaling') .AND.
     &       (xx_genarr3d_preproc_c(k2,iarr).EQ.'log10ctrl') ) THEN
            WRITE(msgBuf,'(A)')
     &      'CTRL_CHECK: conflicting preproc_c & preproc flags '
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A,I2,A)')
     &      'log10ctrl and noscaling set for xx_genarr2d(',iarr,'). '
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R CTRL_CHECK'
          ENDIF
        ENDDO
      ENDDO
#endif

      RETURN
      END
