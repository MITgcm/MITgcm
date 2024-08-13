#include "CTRL_OPTIONS.h"
#include "AD_CONFIG.h"
C--  File ctrl_readparms.F:
C--   Contents
C--   o CTRL_READPARMS
C--   o CTRL_SET_RETIRED_PARMS
C--   o CTRL_CHECK_RETIRED_PARMS

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CTRL_READPARMS
C     !INTERFACE:
      SUBROUTINE CTRL_READPARMS( myThid )

C     !DESCRIPTION:
C     ==================================================================
C     o read ctrl parameters
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "CTRL.h"
# include "CTRL_GENARR.h"
# include "CTRL_OBCS.h"
# include "ctrl_local_params.h"
#endif
#ifdef ALLOW_AUTODIFF
#include "AUTODIFF_PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid

C     == EXTERNAL FUNCTIONS ==
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER iUnit
      INTEGER nRetired
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
      INTEGER iarr,jarr
#endif
      CHARACTER*(MAX_LEN_FNAM) namBuf
      INTEGER IL
#ifdef HAVE_SYSTEM
      INTEGER pIL
#endif
C     retired parameters (others are in ctrl_local_params.h)
      LOGICAL ctrlUseGen
      LOGICAL ctrlSmoothCorrel3D
      LOGICAL ctrlSmoothCorrel2D
CEOP

C--   Read the namelist input.
      NAMELIST /CTRL_NML/
     &  xx_theta_file, xx_salt_file,
     &  xx_hflux_file, xx_hflux_remo_intercept, xx_hflux_remo_slope,
     &  xx_hfluxstartdate1, xx_hfluxstartdate2, xx_hfluxperiod,
     &  xx_sflux_file, xx_sflux_remo_intercept, xx_sflux_remo_slope,
     &  xx_sfluxstartdate1, xx_sfluxstartdate2, xx_sfluxperiod,
     &  xx_tauu_file, xx_tauu_remo_intercept, xx_tauu_remo_slope,
     &  xx_tauustartdate1,  xx_tauustartdate2,  xx_tauuperiod,
     &  xx_tauv_file, xx_tauv_remo_intercept, xx_tauv_remo_slope,
     &  xx_tauvstartdate1,  xx_tauvstartdate2,  xx_tauvperiod,
     &  xx_atemp_file, xx_atemp_remo_intercept, xx_atemp_remo_slope,
     &  xx_atempstartdate1, xx_atempstartdate2, xx_atempperiod,
     &  xx_aqh_file, xx_aqh_remo_intercept, xx_aqh_remo_slope,
     &  xx_aqhstartdate1, xx_aqhstartdate2, xx_aqhperiod,
     &  xx_precip_file, xx_precip_remo_intercept, xx_precip_remo_slope,
     &  xx_precipstartdate1, xx_precipstartdate2, xx_precipperiod,
     &  xx_swflux_file, xx_swflux_remo_intercept, xx_swflux_remo_slope,
     &  xx_swfluxstartdate1, xx_swfluxstartdate2, xx_swfluxperiod,
     &  xx_swdown_file, xx_swdown_remo_intercept, xx_swdown_remo_slope,
     &  xx_swdownstartdate1, xx_swdownstartdate2, xx_swdownperiod,
     &  xx_lwflux_file, xx_lwflux_remo_intercept, xx_lwflux_remo_slope,
     &  xx_lwfluxstartdate1, xx_lwfluxstartdate2, xx_lwfluxperiod,
     &  xx_lwdown_file, xx_lwdown_remo_intercept, xx_lwdown_remo_slope,
     &  xx_lwdownstartdate1, xx_lwdownstartdate2, xx_lwdownperiod,
     &  xx_evap_file, xx_evap_remo_intercept, xx_evap_remo_slope,
     &  xx_evapstartdate1, xx_evapstartdate2, xx_evapperiod,
     &  xx_snowprecip_file, xx_snowprecip_remo_intercept,
     &  xx_snowprecip_remo_slope,  xx_snowprecipperiod,
     &  xx_snowprecipstartdate1, xx_snowprecipstartdate2,
     &  xx_apressure_file, xx_apressure_remo_intercept,
     &  xx_apressure_remo_slope, xx_apressureperiod,
     &  xx_apressurestartdate1, xx_apressurestartdate2,
     &  xx_runoff_file, xx_runoff_remo_intercept, xx_runoff_remo_slope,
     &  xx_runoffstartdate1, xx_runoffstartdate2, xx_runoffperiod,
     &  xx_uwind_file, xx_uwind_remo_intercept, xx_uwind_remo_slope,
     &  xx_uwindstartdate1, xx_uwindstartdate2, xx_uwindperiod,
     &  xx_vwind_file, xx_vwind_remo_intercept, xx_vwind_remo_slope,
     &  xx_vwindstartdate1, xx_vwindstartdate2, xx_vwindperiod,
     &  xx_aqh_mean_file, xx_atemp_mean_file, xx_precip_mean_file,
     &  xx_swdown_mean_file, xx_uwind_mean_file, xx_vwind_mean_file,
     &  xx_obcsn_file,
     &  xx_obcsnstartdate1,  xx_obcsnstartdate2,  xx_obcsnperiod,
     &  xx_obcss_file,
     &  xx_obcssstartdate1,  xx_obcssstartdate2,  xx_obcssperiod,
     &  xx_obcsw_file,
     &  xx_obcswstartdate1,  xx_obcswstartdate2,  xx_obcswperiod,
     &  xx_obcse_file,
     &  xx_obcsestartdate1,  xx_obcsestartdate2,  xx_obcseperiod,
     &  xx_diffkr_file, xx_kapgm_file, xx_kapredi_file, xx_tr1_file,
     &  xx_sst_file, xx_sst_remo_intercept, xx_sst_remo_slope,
     &  xx_sss_file, xx_sss_remo_intercept, xx_sss_remo_slope,
     &  xx_sststartdate1, xx_sststartdate2, xx_sstperiod,
     &  xx_sssstartdate1, xx_sssstartdate2, xx_sssperiod,
     &  xx_depth_file, xx_gen2d_file, xx_gen3d_file,
     &  xx_efluxy_file, xx_efluxp_file, xx_bottomdrag_file,
     &  xx_edtaux_file, xx_edtauy_file,
     &  xx_uvel_file, xx_vvel_file, xx_etan_file,
     &  xx_siarea_file, xx_siheff_file, xx_sihsnow_file,
     &  xx_shifwflx_file,
     &  xx_shifwflx_remo_intercept, xx_shifwflx_remo_slope,
     &  xx_shifwflxstartdate1, xx_shifwflxstartdate2, xx_shifwflxperiod,
     &  doInitXX, ctrlSmoothCorrel3D, ctrlSmoothCorrel2D, ctrlUseGen,
     &  doPackDiag,
     &  doZscaleUnpack, doZscalePack,
     &  doMainUnpack, doMainPack,
     &  doSinglePrecTapelev,
     &  doAdmtlmBypassAD,
     &  delZexp, forcingPrecond, ctrlprec, ctrlDir
cHFLUXM_CONTROL
     &  ,xx_hfluxm_file
cHFLUXM_CONTROL

      NAMELIST /CTRL_PACKNAMES/
     &  yadmark, yctrlid, yctrlposunpack, yctrlpospack,
     &  ctrlname, costname, scalname, maskname, metaname

#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
      NAMELIST /CTRL_NML_GENARR/
     &  xx_genarr2d_file, xx_genarr3d_file, xx_gentim2d_file,
     &  xx_genarr2d_weight, xx_genarr3d_weight, xx_gentim2d_weight,
     &  xx_genarr2d_bounds, xx_genarr3d_bounds, xx_gentim2d_bounds,
     &  genarr2dPrecond, genarr3dPrecond, gentim2dPrecond,
     &  mult_gentim2d,mult_genarr2d,mult_genarr3d,
     &  xx_genarr2d_preproc, xx_genarr2d_preproc_i,
     &  xx_genarr2d_preproc_r, xx_genarr2d_preproc_c,
     &  xx_genarr3d_preproc, xx_genarr3d_preproc_i,
     &  xx_genarr3d_preproc_r, xx_genarr3d_preproc_c,
     &  xx_gentim2d_preproc, xx_gentim2d_preproc_i,
     &  xx_gentim2d_preproc_r, xx_gentim2d_preproc_c,
     &  xx_gentim2d_period,
     &  xx_gentim2d_startdate1, xx_gentim2d_startdate2,
     &  xx_gentim2d_cumsum,xx_gentim2d_glosum
#endif

      IF ( .NOT.useCTRL ) THEN
C-    pkg CTRL is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.ctrl is found
         CALL PACKAGES_UNUSED_MSG( 'useCTRL', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER( myThid )

C--     Set default values.
        doInitXX            = .TRUE.
#ifdef ALLOW_SMOOTH_CORREL3D
        ctrlSmoothCorrel3D  = .TRUE.
#else
        ctrlSmoothCorrel3D  = .FALSE.
#endif
#ifdef ALLOW_SMOOTH_CORREL2D
        ctrlSmoothCorrel2D  = .TRUE.
#else
        ctrlSmoothCorrel2D  = .FALSE.
#endif
        ctrlUseGen          = .TRUE.
#ifdef ALLOW_ADMTLM
        doAdmTlm            = .TRUE.
#else
        doAdmTlm            = .FALSE.
#endif
        doPackDiag          = .FALSE.
        doZscaleUnpack      = .FALSE.
        doZscalePack        = .FALSE.
        doMainUnpack        = .TRUE.
#if (defined (ALLOW_ADJOINT_RUN)||defined (ALLOW_TANGENTLINEAR_RUN))
        doMainPack          = .TRUE.
#else
        doMainPack          = .FALSE.
#endif
        doSinglePrecTapelev = .FALSE.
        doAdmtlmBypassAD    = .FALSE.

        delZexp = 0.
        forcingPrecond = 1. _d 0

#ifdef CTRL_SET_PREC_32
C     for backward compatibility set default value to 32 if this
C     flag is defined
        ctrlprec = 32
#else
        ctrlprec = 64
#endif

        ctrlDir = ' '

        xx_theta_file      = 'xx_theta'
        xx_salt_file       = 'xx_salt'

        xx_gen2d_file      = 'xx_gen2d'
        xx_gen3d_file      = 'xx_gen3d'

#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
        do iarr = 1, maxCtrlArr2D
         write(xx_genarr2d_file(iarr)(1:MAX_LEN_FNAM),'(a)') ' '
#ifdef ALLOW_OPENAD
         write(xx_genarr2d_file(iarr)(1:14),'(a,i2.2)')
     &        'xx_genarr2d_', iarr
         write(xx_genarr2d_file(iarr)(15:MAX_LEN_FNAM),'(a)') ' '
#endif
         write(xx_genarr2d_weight(iarr)(1:MAX_LEN_FNAM),'(a)') ' '
         genarr2dPrecond(iarr)  = 1. _d 0
         mult_genarr2d(iarr)    = 1. _d 0
         do jarr = 1, 5
           xx_genarr2d_bounds(jarr,iarr) = 0. _d 0
         enddo
         do jarr=1,maxCtrlProc
          xx_genarr2d_preproc(jarr,iarr)   = ' '
          xx_genarr2d_preproc_c(jarr,iarr) = ' '
          xx_genarr2d_preproc_i(jarr,iarr) = 0
          xx_genarr2d_preproc_r(jarr,iarr) = 0. _d 0
         enddo
        enddo

        do iarr = 1, maxCtrlArr3D
         write(xx_genarr3d_file(iarr)(1:MAX_LEN_FNAM),'(a)') ' '
#ifdef ALLOW_OPENAD
         write(xx_genarr3d_file(iarr)(1:14),'(a,i2.2)')
     &        'xx_genarr3d_', iarr
         write(xx_genarr3d_file(iarr)(15:MAX_LEN_FNAM),'(a)') ' '
#endif
         write(xx_genarr3d_weight(iarr)(1:MAX_LEN_FNAM),'(a)') ' '
         genarr3dPrecond(iarr)  = 1. _d 0
         mult_genarr3d(iarr)    = 1. _d 0
         do jarr = 1, 5
           xx_genarr3d_bounds(jarr,iarr) = 0. _d 0
         enddo
         do jarr=1,maxCtrlProc
          xx_genarr3d_preproc(jarr,iarr)   = ' '
          xx_genarr3d_preproc_c(jarr,iarr) = ' '
          xx_genarr3d_preproc_i(jarr,iarr) = 0
          xx_genarr3d_preproc_r(jarr,iarr) = 0. _d 0
         enddo
        enddo

        do iarr = 1, maxCtrlTim2D
         write(xx_gentim2d_file(iarr)(1:MAX_LEN_FNAM),'(a)') ' '
#ifdef ALLOW_OPENAD
         write(xx_gentim2d_file(iarr)(1:14),'(a,i2.2)')
     &        'xx_gentim2d_', iarr
         write(xx_gentim2d_file(iarr)(15:MAX_LEN_FNAM),'(a)') ' '
#endif
         write(xx_gentim2d_weight(iarr)(1:MAX_LEN_FNAM),'(a)') ' '
         xx_gentim2d_startdate1(iarr) = 0
         xx_gentim2d_startdate2(iarr) = 0
         xx_gentim2d_period(iarr)     = 0. _d 0
         xx_gentim2d_cumsum(iarr)     = .FALSE.
         xx_gentim2d_glosum(iarr)     = .FALSE.
         do jarr=1,maxCtrlProc
          xx_gentim2d_preproc(jarr,iarr)   = ' '
          xx_gentim2d_preproc_c(jarr,iarr) = ' '
          xx_gentim2d_preproc_i(jarr,iarr) = 0
          xx_gentim2d_preproc_r(jarr,iarr) = 0. _d 0
         enddo
         mult_gentim2d(iarr)          = 1. _d 0
         gentim2dPrecond(iarr)        = 1. _d 0
         do jarr = 1, 5
           xx_gentim2d_bounds(jarr,iarr) = 0. _d 0
         enddo
        enddo
#endif

        xx_obcsnstartdate1  =   0
        xx_obcsnstartdate2  =   0
        xx_obcsnperiod      =   0. _d 0
        xx_obcsn_file       = 'xx_obcsn'

        xx_obcssstartdate1  =   0
        xx_obcssstartdate2  =   0
        xx_obcssperiod      =   0. _d 0
        xx_obcss_file       = 'xx_obcss'

        xx_obcswstartdate1  =   0
        xx_obcswstartdate2  =   0
        xx_obcswperiod      =   0. _d 0
        xx_obcsw_file       = 'xx_obcsw'

        xx_obcsestartdate1  =   0
        xx_obcsestartdate2  =   0
        xx_obcseperiod      =   0. _d 0
        xx_obcse_file       = 'xx_obcse'

C     retired parameters

        CALL CTRL_SET_RETIRED_PARMS(xx_hfluxstartdate1,
     I       xx_hfluxstartdate2, xx_hfluxperiod, xx_hflux_file,
     I       xx_hflux_file, xx_hflux_remo_intercept,
     I       xx_hflux_remo_slope , myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_sfluxstartdate1,
     I       xx_sfluxstartdate2, xx_sfluxperiod, xx_sflux_file,
     I       xx_sflux_file, xx_sflux_remo_intercept,
     I       xx_sflux_remo_slope, myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_tauustartdate1,xx_tauustartdate2,
     I       xx_tauuperiod, xx_tauu_file, xx_tauu_file,
     I       xx_tauu_remo_intercept, xx_tauu_remo_slope, myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_tauvstartdate1,xx_tauvstartdate2,
     I       xx_tauvperiod, xx_tauv_file, xx_tauv_file,
     I       xx_tauv_remo_intercept, xx_tauv_remo_slope, myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_atempstartdate1,
     I       xx_atempstartdate2, xx_atempperiod, xx_atemp_file,
     I       xx_atemp_mean_file, xx_atemp_remo_intercept,
     I       xx_atemp_remo_slope, myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_aqhstartdate1, xx_aqhstartdate2,
     I       xx_aqhperiod, xx_aqh_file, xx_aqh_mean_file,
     I       xx_aqh_remo_intercept, xx_aqh_remo_slope, myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_precipstartdate1,
     I       xx_precipstartdate2, xx_precipperiod, xx_precip_file,
     I       xx_precip_mean_file, xx_precip_remo_intercept,
     I       xx_precip_remo_slope, myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_sfluxstartdate1,
     I       xx_sfluxstartdate2, xx_sfluxperiod, xx_sflux_file,
     I       xx_sflux_file, xx_sflux_remo_intercept,xx_sflux_remo_slope,
     I       myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_swdownstartdate1,
     I       xx_swdownstartdate2, xx_swdownperiod, xx_swdown_file,
     I       xx_swdown_mean_file, xx_swdown_remo_intercept,
     I       xx_swdown_remo_slope, myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_lwfluxstartdate1,
     I       xx_lwfluxstartdate2, xx_lwfluxperiod, xx_lwflux_file,
     I       xx_lwflux_file, xx_lwflux_remo_intercept,
     I       xx_lwflux_remo_slope, myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_lwdownstartdate1,
     I       xx_lwdownstartdate2, xx_lwdownperiod, xx_lwdown_file,
     I       xx_lwdown_file, xx_lwdown_remo_intercept,
     I       xx_lwdown_remo_slope, myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_evapstartdate1,xx_evapstartdate2,
     I       xx_evapperiod, xx_evap_file, xx_evap_file,
     I       xx_evap_remo_intercept, xx_evap_remo_slope, myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_snowprecipstartdate1,
     I       xx_snowprecipstartdate2, xx_snowprecipperiod,
     I       xx_snowprecip_file, xx_snowprecip_file,
     I       xx_snowprecip_remo_intercept, xx_snowprecip_remo_slope,
     I       myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_apressurestartdate1,
     I       xx_apressurestartdate2, xx_apressureperiod,
     I       xx_apressure_file, xx_apressure_file,
     I       xx_apressure_remo_intercept, xx_apressure_remo_slope,
     I       myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_runoffstartdate1,
     I       xx_runoffstartdate2, xx_runoffperiod, xx_runoff_file,
     I       xx_runoff_file, xx_runoff_remo_intercept,
     I       xx_runoff_remo_slope, myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_uwindstartdate1,
     I       xx_uwindstartdate2, xx_uwindperiod, xx_uwind_file,
     I       xx_uwind_file, xx_uwind_remo_intercept,xx_uwind_remo_slope,
     I       myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_vwindstartdate1,
     I       xx_vwindstartdate2, xx_vwindperiod, xx_vwind_file,
     I       xx_vwind_file, xx_vwind_remo_intercept,xx_vwind_remo_slope,
     I       myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_sststartdate1, xx_sststartdate2,
     I       xx_sstperiod, xx_sst_file, xx_sst_file,
     I       xx_sst_remo_intercept, xx_sst_remo_slope, myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_sssstartdate1, xx_sssstartdate2,
     I       xx_sssperiod, xx_sss_file, xx_sss_file,
     I       xx_sss_remo_intercept, xx_sss_remo_slope, myThid )
        CALL CTRL_SET_RETIRED_PARMS(xx_shifwflxstartdate1,
     I       xx_shifwflxstartdate2, xx_shifwflxperiod, xx_shifwflx_file,
     I       xx_shifwflx_file, xx_shifwflx_remo_intercept,
     I       xx_shifwflx_remo_slope, myThid )

        xx_diffkr_file      = ' '
        xx_kapgm_file       = ' '
        xx_kapredi_file     = ' '
        xx_tr1_file         = ' '
        xx_depth_file       = ' '
        xx_efluxy_file      = ' '
        xx_efluxp_file      = ' '
        xx_bottomdrag_file  = ' '
        xx_edtaux_file      = ' '
        xx_edtauy_file      = ' '
        xx_uvel_file        = ' '
        xx_vvel_file        = ' '
        xx_etan_file        = ' '
        xx_siarea_file      = ' '
        xx_siheff_file      = ' '
        xx_sihsnow_file     = ' '
cHFLUXM_CONTROL
        xx_hfluxm_file      = ' '
cHFLUXM_CONTROL
C     end retired parameters

#ifdef ALLOW_TANGENTLINEAR_RUN
        yadprefix           = 'g_'
        yadmark             = 'g_'
#else
        yadprefix           = 'ad'
        yadmark             = 'ad'
#endif
        yctrlid             = 'MIT_CE_000'
        yctrlposunpack      = '.opt'
        yctrlpospack        = '.opt'
        ctrlname            = 'ecco_ctrl'
        costname            = 'ecco_cost'
        scalname            = ' '
        maskname            = ' '
        metaname            = ' '

C--     Next, read the cost data file.
        WRITE(msgBuf,'(A)') 'CTRL_READPARMS: opening data.ctrl'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

        CALL OPEN_COPY_DATA_FILE(
     I                          'data.ctrl', 'CTRL_READPARMS',
     O                          iUnit,
     I                          myThid )

        READ(unit = iUnit, nml = ctrl_nml)
        READ(unit = iUnit, nml = ctrl_packnames)
#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
        READ(unit = iUnit, nml = ctrl_nml_genarr)
#endif

        WRITE(msgBuf,'(A)')
     &       'CTRL_READPARMS: finished reading data.ctrl'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                  SQUEEZE_RIGHT , 1)

#ifdef SINGLE_DISK_IO
        CLOSE(iUnit)
#else
        CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C-- Create ctrlDir if necessary
      IF ( ctrlDir .NE. ' ' ) THEN
       IL = ILNBLNK( ctrlDir )
C      append "/", if necessary
       IF ( IL.LT.MAX_LEN_FNAM .AND. ctrlDir(IL:IL).NE.'/' ) THEN
        namBuf(1:IL) = ctrlDir(1:IL)
        WRITE(ctrlDir(1:IL+1),'(2A)') namBuf(1:IL),'/'
       ENDIF
#ifdef HAVE_SYSTEM
C      create directory
       IL = ILNBLNK( ctrlDir ) -1
       WRITE(namBuf,'(2A)') ' mkdir -p ', ctrlDir(1:IL)
       pIL = 1 + ILNBLNK( namBuf )
       WRITE(standardMessageUnit,'(3A)')
     &  '==> SYSTEM CALL (from CTRL_READPARMS): >',
     &                                 namBuf(1:pIL), '<'
       CALL SYSTEM( namBuf(1:pIL) )
#else
       WRITE(msgBuf,'(2A)') '** WARNING ** CTRL_READPARMS: ',
     &       'cannot call mkdir -> please create ctrlDir manually'
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
#endif
      ENDIF

      IF (ctrlDir .NE. ' ') THEN
       WRITE(msgBuf,'(2A)')
     & 'read-write ctrl files from ', ctrlDir(1:IL)
      ELSE
       WRITE(msgBuf,'(A)')
     & 'read-write ctrl files from current run directory'
      ENDIF
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C-- Check retired parameters
      nRetired = 0
      IF ( ctrlSmoothCorrel2D ) THEN
       WRITE(msgBuf,'(2A)') 'S/R CTRL_READPARMS: ',
     &  'ctrlSmoothCorrel2D no longer allowed in file "data.ctrl"'
       CALL PRINT_ERROR( msgBuf, myThid )
#ifdef ALLOW_SMOOTH_CORREL2D
       WRITE(msgBuf,'(2A)') 'S/R CTRL_READPARMS: ',
     &  ' nor is ALLOW_SMOOTH_CORREL2D allowed to be defined.'
       CALL PRINT_ERROR( msgBuf, myThid )
#endif
       WRITE(msgBuf,'(A)')
     &  'S/R CTRL_READPARMS: Please use the smooth-pkg instead'
       CALL PRINT_ERROR( msgBuf, myThid )
       nRetired = nRetired + 1
      ENDIF
      IF ( ctrlSmoothCorrel3D ) THEN
       WRITE(msgBuf,'(2A)') 'S/R CTRL_READPARMS: ',
     &  'ctrlSmoothCorrel3D no longer allowed in file "data.ctrl"'
       CALL PRINT_ERROR( msgBuf, myThid )
#ifdef ALLOW_SMOOTH_CORREL3D
       WRITE(msgBuf,'(2A)') 'S/R CTRL_READPARMS: ',
     &  ' nor is ALLOW_SMOOTH_CORREL3D allowed to be defined.'
       CALL PRINT_ERROR( msgBuf, myThid )
#endif
       WRITE(msgBuf,'(A)')
     &  'S/R CTRL_READPARMS: Please use the smooth-pkg instead'
       CALL PRINT_ERROR( msgBuf, myThid )
       nRetired = nRetired + 1
      ENDIF
      IF ( .NOT. ctrlUseGen ) THEN
       WRITE(msgBuf,'(A,A)')
     &  'S/R CTRL_READPARMS: ctrlUseGen',
     &  ' is no longer allowed in file "data.ctrl"'
       CALL PRINT_ERROR( msgBuf, myThid )
       nRetired = nRetired + 1
      ENDIF
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_hflux',
     I     xx_hfluxstartdate1, xx_hfluxstartdate2,
     I     xx_hfluxperiod, xx_hflux_file, ' ',
     I     xx_hflux_remo_intercept, xx_hflux_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_sflux',
     I     xx_sfluxstartdate1, xx_sfluxstartdate2,
     I     xx_sfluxperiod, xx_sflux_file, ' ',
     I     xx_sflux_remo_intercept,xx_sflux_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_tauu',
     I     xx_tauustartdate1, xx_tauustartdate2,
     I     xx_tauuperiod, xx_tauu_file, ' ',
     I     xx_tauu_remo_intercept, xx_tauu_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_tauv',
     I     xx_tauvstartdate1,xx_tauvstartdate2,
     I     xx_tauvperiod, xx_tauv_file, ' ',
     I     xx_tauv_remo_intercept, xx_tauv_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_atemp',
     I     xx_atempstartdate1, xx_atempstartdate2,
     I     xx_atempperiod, xx_atemp_file, xx_atemp_mean_file,
     I     xx_atemp_remo_intercept, xx_atemp_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_aqh',
     I     xx_aqhstartdate1, xx_aqhstartdate2,
     I     xx_aqhperiod, xx_aqh_file, xx_aqh_mean_file,
     I     xx_aqh_remo_intercept, xx_aqh_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_precip',
     I     xx_precipstartdate1, xx_precipstartdate2,
     I     xx_precipperiod, xx_precip_file, xx_precip_mean_file,
     I     xx_precip_remo_intercept, xx_precip_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_sflux',
     I     xx_sfluxstartdate1, xx_sfluxstartdate2,
     I     xx_sfluxperiod, xx_sflux_file, ' ',
     I     xx_sflux_remo_intercept,xx_sflux_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS('xx_swdown',
     I     xx_swdownstartdate1, xx_swdownstartdate2,
     I     xx_swdownperiod, xx_swdown_file, xx_swdown_mean_file,
     I     xx_swdown_remo_intercept, xx_swdown_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS('xx_lwflux',
     I     xx_lwfluxstartdate1, xx_lwfluxstartdate2,
     I     xx_lwfluxperiod, xx_lwflux_file, ' ',
     I     xx_lwflux_remo_intercept, xx_lwflux_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS('xx_lwdown',
     I     xx_lwdownstartdate1, xx_lwdownstartdate2,
     I     xx_lwdownperiod, xx_lwdown_file, ' ',
     I     xx_lwdown_remo_intercept, xx_lwdown_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_evap',
     I     xx_evapstartdate1, xx_evapstartdate2,
     I     xx_evapperiod, xx_evap_file, ' ',
     I     xx_evap_remo_intercept, xx_evap_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_snowprecip',
     I     xx_snowprecipstartdate1, xx_snowprecipstartdate2,
     I     xx_snowprecipperiod, xx_snowprecip_file, ' ',
     I     xx_snowprecip_remo_intercept, xx_snowprecip_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_apressure',
     I     xx_apressurestartdate1, xx_apressurestartdate2,
     I     xx_apressureperiod, xx_apressure_file, ' ',
     I     xx_apressure_remo_intercept, xx_apressure_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_runoff',
     I     xx_runoffstartdate1, xx_runoffstartdate2,
     I     xx_runoffperiod, xx_runoff_file, ' ',
     I     xx_runoff_remo_intercept, xx_runoff_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_uwind',
     I     xx_uwindstartdate1, xx_uwindstartdate2,
     I     xx_uwindperiod, xx_uwind_file, ' ',
     I     xx_uwind_remo_intercept,xx_uwind_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_vwind',
     I     xx_vwindstartdate1, xx_vwindstartdate2,
     I     xx_vwindperiod, xx_vwind_file, ' ',
     I     xx_vwind_remo_intercept,xx_vwind_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_sst',
     I     xx_sststartdate1, xx_sststartdate2,
     I     xx_sstperiod, xx_sst_file, ' ',
     I     xx_sst_remo_intercept, xx_sst_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_sss',
     I     xx_sssstartdate1, xx_sssstartdate2,
     I     xx_sssperiod, xx_sss_file, ' ',
     I     xx_sss_remo_intercept, xx_sss_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_shifwflx',
     I     xx_shifwflxstartdate1, xx_shifwflxstartdate2,
     I     xx_shifwflxperiod, xx_shifwflx_file, ' ',
     I     xx_shifwflx_remo_intercept, xx_shifwflx_remo_slope,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_diffkr_file',
     I     UNSET_I, UNSET_I, UNSET_RL, xx_diffkr_file, ' ',
     I     UNSET_RL, UNSET_RL,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_kapgm_file',
     I     UNSET_I, UNSET_I, UNSET_RL, xx_kapgm_file, ' ',
     I     UNSET_RL, UNSET_RL,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_kapredi_file',
     I     UNSET_I, UNSET_I, UNSET_RL, xx_kapredi_file, ' ',
     I     UNSET_RL, UNSET_RL,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_tr1_file',
     I     UNSET_I, UNSET_I, UNSET_RL, xx_tr1_file, ' ',
     I     UNSET_RL, UNSET_RL,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_depth_file',
     I     UNSET_I, UNSET_I, UNSET_RL, xx_depth_file, ' ',
     I     UNSET_RL, UNSET_RL,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_efluxy_file',
     I     UNSET_I, UNSET_I, UNSET_RL, xx_efluxy_file, ' ',
     I     UNSET_RL, UNSET_RL,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_efluxp_file',
     I     UNSET_I, UNSET_I, UNSET_RL, xx_efluxp_file, ' ',
     I     UNSET_RL, UNSET_RL,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_bottomdrag_file',
     I     UNSET_I, UNSET_I, UNSET_RL, xx_bottomdrag_file, ' ',
     I     UNSET_RL, UNSET_RL,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_edtaux_file',
     I     UNSET_I, UNSET_I, UNSET_RL, xx_edtaux_file, ' ',
     I     UNSET_RL, UNSET_RL,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_edtauy_file',
     I     UNSET_I, UNSET_I, UNSET_RL, xx_edtauy_file, ' ',
     I     UNSET_RL, UNSET_RL,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_uvel_file',
     I     UNSET_I, UNSET_I, UNSET_RL, xx_uvel_file, ' ',
     I     UNSET_RL, UNSET_RL,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_vvel_file',
     I     UNSET_I, UNSET_I, UNSET_RL, xx_vvel_file, ' ',
     I     UNSET_RL, UNSET_RL,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_etan_file',
     I     UNSET_I, UNSET_I, UNSET_RL, xx_etan_file, ' ',
     I     UNSET_RL, UNSET_RL,
     U     nRetired,
     I     myThid )
      CALL CTRL_CHECK_RETIRED_PARMS( 'xx_hfluxm_file',
     I     UNSET_I, UNSET_I, UNSET_RL, xx_hfluxm_file, ' ',
     I     UNSET_RL, UNSET_RL,
     U     nRetired,
     I     myThid )

      IF ( nRetired .GT. 0 ) THEN
       WRITE(msgBuf,'(2A)') 'S/R CTRL_READPARMS: ',
     &  'Error reading parameter file "data.ctrl"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,I3,A)') 'S/R CTRL_READPARMS: ', nRetired,
     &      ' out of date parameters were found in namelist CTRL_NML'
       CALL PRINT_ERROR( msgBuf, myThid )
       CALL ALL_PROC_DIE( 0 )
       STOP 'ABNORMAL END: S/R CTRL_READPARMS'
      ENDIF

      _END_MASTER( myThid )

      _BARRIER

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CTRL_SET_RETIRED_PARMS
C     !INTERFACE:
      SUBROUTINE CTRL_SET_RETIRED_PARMS(
     I     xx_startdate1,
     I     xx_startdate2,
     I     xx_period,
     I     xx_file,
     I     xx_mean_file,
     I     xx_remo_intercept,
     I     xx_remo_slope,
     I     myThid )

C     !DESCRIPTION:
C     ==================================================================
C     o set some retired parameters
C     ==================================================================

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER xx_startdate1, xx_startdate2
      _RL xx_period
      CHARACTER*(*) xx_file, xx_mean_file
      _RL xx_remo_intercept, xx_remo_slope
      INTEGER myThid

C     !LOCAL VARIABLES:
CEOP
      xx_startdate1 = UNSET_I
      xx_startdate2 = UNSET_I
      xx_period     = UNSET_RL
      xx_remo_intercept = UNSET_RL
      xx_remo_slope = UNSET_RL
      xx_file = ' '
      xx_mean_file = ' '

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CTRL_CHECK_RETIRED_PARMS
C     !INTERFACE:
      SUBROUTINE CTRL_CHECK_RETIRED_PARMS(
     I     xx_name,
     I     xx_startdate1,
     I     xx_startdate2,
     I     xx_period,
     I     xx_file,
     I     xx_mean_file,
     I     xx_remo_intercept,
     I     xx_remo_slope,
     U     nRetired,
     I     myThid )

C     !DESCRIPTION:
C     ==================================================================
C     o check some retired parameter values
C     ==================================================================

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
      CHARACTER*(*) xx_name
      INTEGER xx_startdate1, xx_startdate2
      _RL xx_period
      CHARACTER*(*) xx_file, xx_mean_file
      _RL xx_remo_intercept, xx_remo_slope
      INTEGER nRetired
      INTEGER myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER il
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF (      xx_startdate1 .NE. UNSET_I
     &     .OR. xx_startdate2 .NE. UNSET_I
     &     .OR. xx_period .NE. UNSET_RL
     &     .OR. xx_remo_intercept .NE. UNSET_RL
     &     .OR. xx_remo_slope .NE. UNSET_RL
     &     .OR. xx_file .NE. ' '
     &     .OR. xx_mean_file .NE. ' ' ) THEN
       il = ILNBLNK(xx_name)
       WRITE(msgBuf,'(A,A,A)')
     &  'S/R CTRL_READPARMS: ', xx_name(1:il),
     &  '* is no longer allowed in file "data.ctrl"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'S/R CTRL_READPARMS: use generic controls instead'
       CALL PRINT_ERROR( msgBuf, myThid )
       nRetired = nRetired + 1
      ENDIF

      RETURN
      END
