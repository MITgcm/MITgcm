#include "CTRL_OPTIONS.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#ifdef ALLOW_SHELFICE
# include "SHELFICE_OPTIONS.h"
#endif
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

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
#include "CTRL_SIZE.h"
#include "CTRL.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif
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
#if ( defined ALLOW_GENARR2D_CONTROL || defined ALLOW_GENARR3D_CONTROL || defined ALLOW_GENTIM2D_CONTROL )
      INTEGER iarr
#endif
#if ( defined ALLOW_GENARR2D_CONTROL || defined ALLOW_GENARR3D_CONTROL )
      INTEGER k2
#endif
#if ( defined ALLOW_GENARR3D_CONTROL && defined ALLOW_PTRACERS )
      INTEGER iLen, iPtr, ascii_1
#endif
#ifdef ECCO_CTRL_DEPRECATED
      INTEGER ioUnit
      CHARACTER*(16) w_sfx
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

#ifdef ALLOW_GENTIM2D_CONTROL
      IF ( .NOT.useCAL ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** CTRL_CHECK: ',
     &    'ALLOW_GENTIM2D_CONTROL is unsafe'
     &    //' without pkg/cal (currently: useCAL=F)'
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
#  ifdef ALLOW_OBCS_CONTROL
      WRITE(msgBuf,'(2A)')
     &        'ALLOW_PACKUNPACK_METHOD2 does not work with',
     &        'open boundary controls (see verif/obcs_ctrl).'
      CALL PRINT_ERROR( msgBuf, myThid )

# endif /* ndef EXCLUDE_CTRL_PACK */
#endif /* ALLOW_PACKUNPACK_METHOD2 */

#if ( !defined ALLOW_OBCS_NORTH && defined ALLOW_OBCSN_CONTROL )
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &     'CPP-flag ALLOW_OBCSN_CONTROL is defined,'
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &     'while CPP-flag ALLOW_OBCS_NORTH is not defined.'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
#endif

#if ( !defined ALLOW_OBCS_SOUTH && defined ALLOW_OBCSS_CONTROL )
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &     'CPP-flag ALLOW_OBCSS_CONTROL is defined,'
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &     'while CPP-flag ALLOW_OBCS_SOUTH is not defined.'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
#endif

#if ( !defined ALLOW_OBCS_WEST && defined ALLOW_OBCSW_CONTROL )
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &     'CPP-flag ALLOW_OBCSW_CONTROL is defined,'
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &     'while CPP-flag ALLOW_OBCS_WEST is not defined.'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
#endif

#if ( !defined ALLOW_OBCS_EAST && defined ALLOW_OBCSE_CONTROL )
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &     'CPP-flag ALLOW_OBCSE_CONTROL is defined,'
      CALL PRINT_ERROR( msgBuf, myThid )
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &     'while CPP-flag ALLOW_OBCS_EAST is not defined.'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
#endif

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
      errCount = errCount + 1
#endif /* ALLOW_HFLUXM_CONTROL */

#ifdef ALLOW_HFACC_CONTROL
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &     'CPP-flag ALLOW_HFACC_CONTROL has been retired.'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      errCount = errCount + 1
#endif
#ifdef ALLOW_HFACC3D_CONTROL
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: ',
     &     'CPP-flag ALLOW_HFACC3D_CONTROL has been retired.'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      errCount = errCount + 1
#endif

#ifdef ALLOW_GENARR2D_CONTROL
C     doscaling must be true to do log10 ctrl
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
            errCount = errCount + 1
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
          errCount = errCount + 1
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
          errCount = errCount + 1
        ENDIF
      ENDDO
#  endif /* SHI_ALLOW_GAMMAFRICT */
# endif /* ALLOW_SHELFICE */
#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
C     doscaling must be true to do log10 ctrl
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
            errCount = errCount + 1
          ENDIF
        ENDDO
      ENDDO
#endif

#ifdef ECCO_CTRL_DEPRECATED
      WRITE(msgBuf,'(2A)') 'CTRL_CHECK: CPP-flag ECCO_CTRL_DEPRECATED',
     &                     ' is no longer available.'
      CALL PRINT_ERROR( msgBuf, myThid )
      ioUnit = errorMessageUnit
      w_sfx = '** WARNING ** : '
      WRITE(msgBuf,'(2A)') '** WARNING ** CTRL_CHECK: ',
     &     'Along with ECCO_CTRL_DEPRECATED a long list'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') '** WARNING ** CTRL_CHECK: ',
     &     ' of CPP-flags have no effect anymore:'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
C---- long list starts
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_APRESSURE_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_APRESSURE_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_AQH_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_ARGO_SALT_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_ARGO_THETA_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_ATEMP_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_ATM_MEAN_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_AUTODIFF_INIT_OLD'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_BOTTOMDRAG_CONTROL_NONGENERIC'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_BOTTOMDRAG_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_BP_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_CTDS_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_CTDT_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_CTRL_SMOOTH'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_DAILYSCAT_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_DAILYSST_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c     WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_diffkr_COST_CONTRIBUTION'
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_DIFFKR_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_DRIFTER_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_DRIFTW_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c     WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_ECCO_DIAGNOSTIC_RUN'
c     WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_ECCO_FORWARD_RUN'
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_EDDYPSI_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_EFLUXY0_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c     WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_EGM96_ERROR_COV'
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_EGM96_ERROR_DIAG'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_ETAN0_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_EVAP_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_EVAP_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_GEN2D_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_GEN3D_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c     WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_GRADIENT_CHECK'
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_HFLUX_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_IESTAU_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_KAPGM_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_KAPREDI_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_LWDOWN_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_LWDOWN_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_LWFLUX_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_LWFLUX_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_MEAN_HFLUX_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_NEW_SSH_COST'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c     WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_NO_DYNAMICS'
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_PRECIP_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_PRECIP_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c     WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_READ_EGM_DATA'
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_RELAXSSS_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_RELAXSST_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_RUNOFF_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_RUNOFF_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SALT0_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SALT_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SCAT_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SFLUX_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SMOOTH_BC_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SMOOTH_CTRL3D'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SNOWPRECIP_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SNOWPRECIP_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c     WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SPH_PROJECTION'
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SSH_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SSH_ERSANOM_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SSH_GFOANOM_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SSH_MEAN_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SSH_TOT'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SSH_TPANOM_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SSHV4_COST'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SSS_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SSS_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SST_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SST_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c     WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_STEPPING_CALL'
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_STRESS_MEAN_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SWDOWN_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SWDOWN_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SWFLUX_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_SWFLUX_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_THETA0_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_THETA_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_TMI_SST_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_TRANSPORT_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_USTRESS_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_UVEL0_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_UWIND_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_UWIND_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_VSTRESS_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_VVEL0_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_VWIND_CONTROL'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_VWIND_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_XBT_COST_CONTRIBUTION'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c     WRITE(msgBuf,'(2A)') w_sfx, 'ALLOW_YMDS_TREE'
c     WRITE(msgBuf,'(2A)') w_sfx, 'APPLY_HFLUX_COST_CONTRIBUTION'
c     WRITE(msgBuf,'(2A)') w_sfx, 'APPLY_SALT_COST_CONTRIBUTION'
c     WRITE(msgBuf,'(2A)') w_sfx, 'APPLY_SFLUX_COST_CONTRIBUTION'
c     WRITE(msgBuf,'(2A)') w_sfx, 'APPLY_SSH_COST_CONTRIBUTION'
c     WRITE(msgBuf,'(2A)') w_sfx, 'APPLY_SST_COST_CONTRIBUTION'
c     WRITE(msgBuf,'(2A)') w_sfx, 'APPLY_THETA_COST_CONTRIBUTION'
c     WRITE(msgBuf,'(2A)') w_sfx, 'APPLY_USTRESS_COST_CONTRIBUTION'
c     WRITE(msgBuf,'(2A)') w_sfx, 'APPLY_VSTRESS_COST_CONTRIBUTION'
      WRITE(msgBuf,'(2A)') w_sfx, 'DISABLE_CTRL_THETA_LIMIT'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') w_sfx, 'GENERIC_BAR_MONTH'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
C---- long list ends
      WRITE(msgBuf,'(2A)') '** WARNING ** CTRL_CHECK: ',
     &     'end of CPP-flag list that have no effect anymore.'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      errCount = errCount + 1
#endif /* ECCO_CTRL_DEPRECATED */

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

      RETURN
      END
