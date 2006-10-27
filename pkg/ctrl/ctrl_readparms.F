C
C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_readparms.F,v 1.25 2006/10/27 05:16:54 heimbach Exp $
C $Name:  $

#include "AD_CONFIG.h"
#include "CTRL_CPPOPTIONS.h"


      subroutine ctrl_readparms( mythid )

c     ==================================================================
c     SUBROUTINE ctrl_readparms
c     ==================================================================
c
c     o read ctrl parameters
c       split from ctrl_init
c
c     started: heimbach@mit.edu 12-Jun-2003
c
c     ==================================================================
c     SUBROUTINE ctrl_readparms
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ctrl.h"

#ifdef ALLOW_OBCS_CONTROL
# include "OBCS.h"
#endif

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer ntmp
      integer ivarindex
      integer iUnit
      integer iobcs
      integer il
      integer errio
      integer startrec
      integer endrec
      integer difftime(4)
      _RL     diffsecs
      _RL     dummy

      character*(80)   ymaskobcs
      character*(max_len_prec) record
      character*(max_len_mbuf) msgbuf

      integer nwetc3d

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c--   Read the namelist input.
      namelist /ctrl_nml/
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
     &  xx_obcsn_file,
     &  xx_obcsnstartdate1,  xx_obcsnstartdate2,  xx_obcsnperiod,
     &  xx_obcss_file,
     &  xx_obcssstartdate1,  xx_obcssstartdate2,  xx_obcssperiod,
     &  xx_obcsw_file,
     &  xx_obcswstartdate1,  xx_obcswstartdate2,  xx_obcswperiod,
     &  xx_obcse_file,
     &  xx_obcsestartdate1,  xx_obcsestartdate2,  xx_obcseperiod,
     &  xx_diffkr_file, xx_kapgm_file, xx_tr1_file,
     &  xx_sst_file, xx_sss_file,
     &  xx_sststartdate1, xx_sststartdate2, xx_sstperiod,
     &  xx_sssstartdate1, xx_sssstartdate2, xx_sssperiod,
     &  xx_depth_file,
     &  xx_efluxy_file, xx_efluxp_file,
     &  xx_bottomdrag_file,
     &  xx_edtaux_file, xx_edtauy_file,
     &  xx_uvel_file, xx_vvel_file, xx_etan_file,
     &  doInitXX,
     &  doPackDiag,
     &  doZscaleUnpack, doZscalePack,
     &  doMainUnpack, doMainPack,
     &  doAdmtlmBypassAD,
     &  delZexp

      namelist /ctrl_packnames/
     &  yadmark, yctrlid, yctrlposunpack, yctrlpospack,
     &  ctrlname, costname, scalname, maskname, metaname

      _BEGIN_MASTER( myThid )

c--     Set default values.
        doInitXX            = .TRUE.
#ifdef ALLOW_ADMTLM
        doAdmTlm            = .TRUE.
#else
        doAdmTlm            = .FALSE.
#endif
        doPackDiag          = .FALSE.
        doZscaleUnpack      = .FALSE.
        doZscalePack        = .FALSE.
        doMainUnpack        = .TRUE.
        doMainPack          = .TRUE.
        doAdmtlmBypassAD    = .FALSE.

        delZexp = 0.

        xx_theta_file      = 'xx_theta'
        xx_salt_file       = 'xx_salt'
c
        xx_hfluxstartdate1 =   0
        xx_hfluxstartdate2 =   0
        xx_hfluxperiod     =   0. _d 0
        xx_hflux_file      = 'xx_hfl'
        xx_hflux_remo_intercept =   0. _d 0
        xx_hflux_remo_slope =   0. _d 0
c
        xx_sfluxstartdate1 =   0
        xx_sfluxstartdate2 =   0
        xx_sfluxperiod     =   0. _d 0
        xx_sflux_file      = 'xx_sfl'
        xx_sflux_remo_intercept =   0. _d 0
        xx_sflux_remo_slope =   0. _d 0
c
        xx_tauustartdate1  =   0
        xx_tauustartdate2  =   0
        xx_tauuperiod      =   0. _d 0
        xx_tauu_file       = 'xx_tauu'
        xx_tauu_remo_intercept =   0. _d 0
        xx_tauu_remo_slope =   0. _d 0
c
        xx_tauvstartdate1  =   0
        xx_tauvstartdate2  =   0
        xx_tauvperiod      =   0. _d 0
        xx_tauv_file       = 'xx_tauv'
        xx_tauv_remo_intercept =   0. _d 0
        xx_tauv_remo_slope =   0. _d 0
c
        xx_atempstartdate1  =   0
        xx_atempstartdate2  =   0
        xx_atempperiod      =   0. _d 0
        xx_atemp_file       = 'xx_atemp'
        xx_atemp_remo_intercept =   0. _d 0
        xx_atemp_remo_slope =   0. _d 0
c
        xx_aqhstartdate1    =   0
        xx_aqhstartdate2    =   0
        xx_aqhperiod        =   0. _d 0
        xx_aqh_file         = 'xx_aqh'
        xx_aqh_remo_intercept =   0. _d 0
        xx_aqh_remo_slope =   0. _d 0
c
        xx_precipstartdate1 =   0
        xx_precipstartdate2 =   0
        xx_precipperiod     =   0. _d 0
        xx_precip_file      = 'xx_precip'
        xx_precip_remo_intercept =   0. _d 0
        xx_precip_remo_slope =   0. _d 0
c
        xx_swfluxstartdate1 =   0
        xx_swfluxstartdate2 =   0
        xx_swfluxperiod     =   0. _d 0
        xx_swflux_file      = 'xx_swflux'
        xx_swflux_remo_intercept =   0. _d 0
        xx_swflux_remo_slope =   0. _d 0
c
        xx_swdownstartdate1 =   0
        xx_swdownstartdate2 =   0
        xx_swdownperiod     =   0. _d 0
        xx_swdown_file      = 'xx_swdown'
        xx_swdown_remo_intercept =   0. _d 0
        xx_swdown_remo_slope =   0. _d 0
c
        xx_lwfluxstartdate1 =   0
        xx_lwfluxstartdate2 =   0
        xx_lwfluxperiod     =   0. _d 0
        xx_lwflux_file      = 'xx_lwflux'
        xx_lwflux_remo_intercept =   0. _d 0
        xx_lwflux_remo_slope =   0. _d 0
c
        xx_lwdownstartdate1 =   0
        xx_lwdownstartdate2 =   0
        xx_lwdownperiod     =   0. _d 0
        xx_lwdown_file      = 'xx_lwdown'
        xx_lwdown_remo_intercept =   0. _d 0
        xx_lwdown_remo_slope =   0. _d 0
c
        xx_evapstartdate1 =   0
        xx_evapstartdate2 =   0
        xx_evapperiod     =   0. _d 0
        xx_evap_file      = 'xx_evap'
        xx_evap_remo_intercept =   0. _d 0
        xx_evap_remo_slope =   0. _d 0
c
        xx_snowprecipstartdate1 =   0
        xx_snowprecipstartdate2 =   0
        xx_snowprecipperiod     =   0. _d 0
        xx_snowprecip_file      = 'xx_snowprecip'
        xx_snowprecip_remo_intercept =   0. _d 0
        xx_snowprecip_remo_slope =   0. _d 0
c
        xx_apressurestartdate1 =   0
        xx_apressurestartdate2 =   0
        xx_apressureperiod     =   0. _d 0
        xx_apressure_file      = 'xx_apressure'
        xx_apressure_remo_intercept =   0. _d 0
        xx_apressure_remo_slope =   0. _d 0
c
        xx_runoffstartdate1 =   0
        xx_runoffstartdate2 =   0
        xx_runoffperiod     =   0. _d 0
        xx_runoff_file      = 'xx_runoff'
        xx_runoff_remo_intercept =   0. _d 0
        xx_runoff_remo_slope =   0. _d 0
c
        xx_uwindstartdate1  =   0
        xx_uwindstartdate2  =   0
        xx_uwindperiod      =   0. _d 0
        xx_uwind_file       = 'xx_uwind'
        xx_uwind_remo_intercept =   0. _d 0
        xx_uwind_remo_slope =   0. _d 0
c
        xx_vwindstartdate1  =   0
        xx_vwindstartdate2  =   0
        xx_vwindperiod      =   0. _d 0
        xx_vwind_file       = 'xx_vwind'
        xx_vwind_remo_intercept =   0. _d 0
        xx_vwind_remo_slope =   0. _d 0
c
        xx_obcsnstartdate1  =   0
        xx_obcsnstartdate2  =   0
        xx_obcsnperiod      =   0. _d 0
        xx_obcsn_file       = 'xx_obcsn'
c
        xx_obcssstartdate1  =   0
        xx_obcssstartdate2  =   0
        xx_obcssperiod      =   0. _d 0
        xx_obcss_file       = 'xx_obcss'
c
        xx_obcswstartdate1  =   0
        xx_obcswstartdate2  =   0
        xx_obcswperiod      =   0. _d 0
        xx_obcsw_file       = 'xx_obcsw'
c
        xx_obcsestartdate1  =   0
        xx_obcsestartdate2  =   0
        xx_obcseperiod      =   0. _d 0
        xx_obcse_file       = 'xx_obcse'
c
        xx_sststartdate1    =   0
        xx_sststartdate2    =   0
        xx_sstperiod        =   0. _d 0
        xx_sst_file         = 'xx_sst'
c
        xx_sssstartdate1    =   0
        xx_sssstartdate2    =   0
        xx_sssperiod        =   0. _d 0
        xx_sss_file         = 'xx_sss'
c
        xx_diffkr_file      = 'xx_diffkr'
        xx_kapgm_file       = 'xx_kapgm'
        xx_tr1_file         = 'xx_ptr'
        xx_depth_file       = 'xx_depth'
        xx_efluxy_file      = 'xx_efluxy'
        xx_efluxp_file      = 'xx_efluxp'
        xx_bottomdrag_file  = 'xx_bottomdrag'
        xx_edtaux_file      = 'xx_edtaux'
        xx_edtauy_file      = 'xx_edtauy'
        xx_uvel_file        = 'xx_uvel'
        xx_vvel_file        = 'xx_vvel'
        xx_etan_file        = 'xx_etan'

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
        ctrlname            = ' '
        costname            = ' '
        scalname            = ' '
        maskname            = ' '
        metaname            = ' '

c--     Next, read the cost data file.
        WRITE(msgBuf,'(A)') 'CTRL_READPARMS: opening data.ctrl'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

        CALL OPEN_COPY_DATA_FILE(
     I                          'data.ctrl', 'CTRL_READPARMS',
     O                          iUnit,
     I                          myThid )

        READ(unit = iUnit, nml = ctrl_nml)
        READ(unit = iUnit, nml = ctrl_packnames)

        WRITE(msgBuf,'(A)') 
     &       'CTRL_READPARMS: finished reading data.ctrl'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                  SQUEEZE_RIGHT , 1)

        CLOSE( iUnit )

      _END_MASTER( myThid )

      _BARRIER

      return
      end

