#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine ecco_readparms( myThid )

c     ==================================================================
c     SUBROUTINE ecco_readparms
c     ==================================================================
c
c     o This routine initialises the package cost.
c     started: Ralf Giering 18-Jan-2001
c
c     ==================================================================
c     SUBROUTINE ecco_readparms
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"

#ifdef ALLOW_ECCO
# ifdef ECCO_CTRL_DEPRECATED
#  include "ecco_cost.h"
# else
#  include "ECCO_SIZE.h"
#  include "ECCO.h"
#  include "ecco_local_params.h"
# endif
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OBCS.h"
#endif
#ifdef ALLOW_CAL
# include "cal.h"
#endif

c     == routine arguments ==
      INTEGER myThid

c     == external functions ==
      INTEGER  ilnblnk
      EXTERNAL ilnblnk

c     == local variables ==
C     msgBuf      - Informational/error message buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER k, k2, iUnit
#ifdef ALLOW_GENCOST_CONTRIBUTION
      INTEGER IL, kk, gencost_k3d, gencost_msk_k3d, ioUnit
      CHARACTER*(128) tempfile
      LOGICAL exst
      _RS     dummyRS(1)
#endif

Catn-- retired parameters
      CHARACTER*(MAX_LEN_FNAM) topexmeanfile
      CHARACTER*(2) cost_yftype
      INTEGER nRetired

c     == end of interface ==

c--   Read the namelist input.
      namelist /ecco_cost_nml/
     &                   data_errfile,
     &                   tbarfile, sbarfile,sstbarfile,
     &                   psbarfile, bpbarfile,iestaubarfile,
     &                   ubarfile, vbarfile, wbarfile,
     &                   tauxbarfile, tauybarfile,
     &                   hfluxmeanbarfile, sfluxmeanbarfile,
     &                   costTranspDataFile, mdtdatfile,
     &                   mdtstartdate1, mdtstartdate2,
     &                   mdtenddate1, mdtenddate2,
     &                   using_cost_altim,
     &                   using_cost_bp, using_cost_scat,
     &                   using_cost_sst, using_cost_seaice,
     &                   using_cost_transp,
     &                   using_mdt, using_tpj, using_ers, using_gfo,
     &                   using_topex,
     &                   topexstartdate1, topexstartdate2,
     &                   topexperiod, topexfile, tpTimeMaskFile,
     &                   topexintercept, topexslope, ersTimeMaskFile,
     &                   ersstartdate1, ersstartdate2, gfoTimeMaskFile,
     &                   ersperiod, ersfile,
     &                   ersintercept, ersslope,
     &                   gfostartdate1, gfostartdate2,
     &                   gfoperiod, gfofile,
     &                   gfointercept, gfoslope,
     &                   scatstartdate1, scatstartdate2,
     &                   scatperiod, scatxdatfile, scatydatfile,
     &                   scatx_errfile, scaty_errfile,
     &                   ssh_errfile, sshv4cost_scalefile,
     &                   sshv4cost_errfile, sshv4cost_errfactor,
     &                   tp_errfile, ers_errfile, gfo_errfile,
     &                   tmistartdate1, tmistartdate2, tmidatfile,
     &                   sststartdate1, sststartdate2, sstdatfile,
     &                   sssstartdate1, sssstartdate2, sssdatfile,
     &                   bpstartdate1, bpstartdate2, bpdatfile,
     &                   iesstartdate1, iesstartdate2, iesdatfile,
     &                   tdatfile, sdatfile, ctdtfile, ctdsfile,
     &                   ctdtclimfile, ctdsclimfile, xbtfile,
     &                   argotstartdate1, argotstartdate2, argotfile,
     &                   argosstartdate1, argosstartdate2, argosfile,
     &                   udriftfile, vdriftfile,
     &                   udrifterrfile, vdrifterrfile,
     &                   curmtrufile,curmtrvfile,
     &                   salterrfile, temperrfile, velerrfile,
     &                   salt0errfile, temp0errfile, vel0errfile,
     &                   etan0errfile, uvel0errfile, vvel0errfile,
     &                   ssterrfile, ssserrfile, bperrfile,ieserrfile,
     &                   hflux_errfile, sflux_errfile,
     &                   tauu_errfile, tauv_errfile,
     &                   atemp_errfile, aqh_errfile,
     &                   precip_errfile, swflux_errfile, swdown_errfile,
     &                   lwflux_errfile, lwdown_errfile, evap_errfile,
     &                   snowprecip_errfile, apressure_errfile,
     &                   uwind_errfile, vwind_errfile,
     &                   geoid_errfile, geoid_covariancefile,
     &                   bottomdrag_errfile,edtau_errfile,
     &                   kapgm_errfile,kapredi_errfile,diffkr_errfile,
     &                   usercost_datafile, usercost_errfile,
     &                   mult_hflux, mult_sflux, mult_tauu, mult_tauv,
     &                   mult_hfluxmm, mult_sfluxmm,
     &                   mult_atemp, mult_aqh,
     &                   mult_precip, mult_swflux, mult_swdown,
     &                   mult_lwflux, mult_lwdown, mult_evap,
     &                   mult_snowprecip, mult_apressure,
     &                   mult_uwind, mult_vwind,
     &                   mult_hmean, mult_h, mult_sshv4cost,
     &                   mult_tp, mult_ers, mult_gfo,
     &                   mult_temp, mult_salt,
     &                   mult_tmi, mult_sst, mult_sss, mult_bp,
     &                   mult_ctdt, mult_ctds, mult_ies,
     &                   mult_ctdtclim, mult_ctdsclim,
     &                   mult_xbt, mult_drift,
     &                   mult_argot, mult_argos,
     &                   mult_scatx, mult_scaty,
     &                   mult_sdrift, mult_tdrift, mult_wdrift,
     &                   mult_temp0, mult_salt0,
     &                   mult_etan0, mult_uvel0, mult_vvel0,
#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
     &                   mult_obcsn, mult_obcss,
     &                   mult_obcsw, mult_obcse, mult_obcsvol,
     &                   mult_ageos,
#endif
     &                   mult_curmtr,
     &                   mult_kapgm, mult_kapredi, mult_diffkr,
     &                   mult_edtau, mult_bottomdrag,
     &                   mult_usercost, mult_transp,
     &                   mult_smooth_ic, mult_smooth_bc,
     &                   whflux0, wsflux0, wtau0,
     &                   wbottomdrag0,
     &                   watemp0, waqh0,
     &                   wprecip0, wswflux0, wswdown0, wwind0,
     &                   wsnowprecip0, wlwflux0, wlwdown0, wevap0,
     &                   wapressure0, wdiffkr0, wkapgm0, wedtau0,
     &                   wkapredi0, wmean_hflux, wmean_sflux, wmean_tau,
     &                   wmean_atemp, wmean_aqh, wmean_precip,
     &                   wmean_swflux, wmean_swdown, wmean_wind,
     &                   wmean_lwflux, wmean_lwdown, wmean_evap,
     &                   wmean_snowprecip, wmean_apressure,
     &                   cost_iprec, ecco_output_sterGloH,
     &                   ecco_keepTSeriesOutp_open,
     &                   cost_yftype, topexmeanfile

#ifdef ALLOW_GENCOST_CONTRIBUTION
      namelist /ecco_gencost_nml/
     &         using_gencost,
     &         gencost_barfile,
     &         gencost_datafile,
     &         gencost_name,
     &         gencost_scalefile,
     &         gencost_errfile,
     &         gencost_itracer,
     &         gencost_kLev_select,
     &         gencost_preproc,
     &         gencost_preproc_c,
     &         gencost_preproc_i,
     &         gencost_preproc_r,
     &         gencost_posproc,
     &         gencost_posproc_c,
     &         gencost_posproc_i,
     &         gencost_posproc_r,
     &         gencost_outputlevel,
     &         gencost_mask,
     &         gencost_spmin,
     &         gencost_spmax,
     &         gencost_spzero,
     &         gencost_wei1d,
     &         gencost_avgperiod,
     &         gencost_nrecperiod,
     &         gencost_startdate1,
     &         gencost_startdate2,
     &         gencost_enddate1,
     &         gencost_enddate2,
     &         gencost_smooth2Ddiffnbt,
     &         gencost_is1d,
     &         gencost_is3d,
     &         gencost_msk_is3d,
     &         gencost_useDensityMask,
     &         gencost_refPressure,
     &         gencost_sigmaLow,
     &         gencost_sigmaHigh,
     &         gencost_tanhScale,
     &         gencost_timevaryweight,
     &         mult_gencost
#endif /* ALLOW_GENCOST_CONTRIBUTION */

      IF ( .NOT.useECCO ) THEN
C-    pkg ECCO is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.ecco is found
         CALL PACKAGES_UNUSED_MSG( 'useECCO', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER( myThid )

c--     Set default values.

c run time switches for main cost terms
#if (defined (ALLOW_SSH_COST_CONTRIBUTION) && \
     !defined (ALLOW_GENCOST_CONTRIBUTION) )
      using_cost_altim=.TRUE.
#else
      using_cost_altim=.FALSE.
#endif
#ifdef ALLOW_SEAICE_COST_CONTRIBUTION
      using_cost_seaice=.TRUE.
#else
      using_cost_seaice=.FALSE.
#endif

c- set default to false
      using_cost_transp=.FALSE.

#ifdef ALLOW_BP_COST_CONTRIBUTION
      using_cost_bp=.TRUE.
#else
      using_cost_bp=.FALSE.
#endif
#ifdef ALLOW_SST_COST_CONTRIBUTION
      using_cost_sst=.TRUE.
#else
      using_cost_sst=.FALSE.
#endif
#if (defined (ALLOW_SCAT_COST_CONTRIBUTION) || \
     defined (ALLOW_DAILYSCAT_COST_CONTRIBUTION) )
      using_cost_scat=.TRUE.
#else
      using_cost_scat=.FALSE.
#endif

c other run time parameters
        data_errfile          =    ' '
        tbarfile              =    'tbar'
        sbarfile              =    'sbar'
        sstbarfile            =    'sstbar'
        psbarfile             =    'psbar'
        bpbarfile             =    'bpbar'
        iestaubarfile         =    'iestaubar'
        ubarfile              =    'ubar'
        vbarfile              =    'vbar'
        wbarfile              =    'wbar'
        tauxbarfile           =    'tauxbar'
        tauybarfile           =    'tauybar'
        hfluxmeanbarfile      =    'hfluxmeanbar'
        sfluxmeanbarfile      =    'sfluxmeanbar'
        costTranspDataFile    =    ' '
        using_tpj             = .false.
        topexstartdate1       =      0
        topexstartdate2       =      0
        topexperiod           =      0. _d 0
        topexintercept        =      0. _d 0
        topexslope            =      0. _d 0
        topexfile             =    ' '
catn: defaulting mdt[start,end]date1 to pavlis so that
catn  eccov4 will run without needing to read in these params
        using_mdt             =.FALSE.
        mdtdatfile            =    ' '
        mdtstartdate1         =      19930101
        mdtstartdate2         =      0
        mdtenddate1           =      20041231
        mdtenddate2           =      0
        using_ers             = .false.
        ersstartdate1         =      0
        ersstartdate2         =      0
        ersperiod             =      0. _d 0
        ersintercept          =      0. _d 0
        ersslope              =      0. _d 0
        ersfile               =    ' '
        using_gfo             = .false.
        gfostartdate1         =      0
        gfostartdate2         =      0
        gfoperiod             =      0. _d 0
        gfointercept          =      0. _d 0
        gfoslope              =      0. _d 0
        gfofile               =    ' '
        scatstartdate1        =      0
        scatstartdate2        =      0
        scatperiod            =      0. _d 0
        scatxdatfile          =    ' '
        scatydatfile          =    ' '
        ssh_errfile           =    ' '
        tp_errfile            =    ' '
        ers_errfile           =    ' '
        gfo_errfile           =    ' '
        scatx_errfile         =    ' '
        scaty_errfile         =    ' '
        tmistartdate1         =      0
        tmistartdate2         =      0
        tmidatfile            =    ' '
        sststartdate1         =      0
        sststartdate2         =      0
        sstdatfile            =    ' '
        sssstartdate1         =      0
        sssstartdate2         =      0
        sssdatfile            =    ' '
        bpstartdate1          =      0
        bpstartdate2          =      0
        bpdatfile             =    ' '
        iesstartdate1         =      0
        iesstartdate2         =      0
        iesdatfile            =    ' '
        tdatfile              =    ' '
        sdatfile              =    ' '
        ctdtfile              =    ' '
        ctdsfile              =    ' '
        ctdtclimfile          =    ' '
        ctdsclimfile          =    ' '
        curmtrufile           =    ' '
        curmtrvfile           =    ' '
        xbtfile               =    ' '
        argotstartdate1       =      0
        argotstartdate2       =      0
        argotfile             =    ' '
        argosstartdate1       =      0
        argosstartdate2       =      0
        argosfile             =    ' '
        udriftfile            =    ' '
        vdriftfile            =    ' '
        udrifterrfile         =    ' '
        vdrifterrfile         =    ' '
        salterrfile           =    ' '
        temperrfile           =    ' '
        velerrfile            =    ' '
        salt0errfile          =    ' '
        temp0errfile          =    ' '
        etan0errfile          =    ' '
        uvel0errfile          =    ' '
        vvel0errfile          =    ' '
        vel0errfile           =    ' '
        ssterrfile            =    ' '
        ssserrfile            =    ' '
        bperrfile             =    ' '
        ieserrfile            =    ' '
        hflux_errfile         =    ' '
        sflux_errfile         =    ' '
        tauu_errfile          =    ' '
        tauv_errfile          =    ' '
        atemp_errfile         =    ' '
        aqh_errfile           =    ' '
        precip_errfile        =    ' '
        swflux_errfile        =    ' '
        swdown_errfile        =    ' '
        lwflux_errfile        =    ' '
        lwdown_errfile        =    ' '
        evap_errfile          =    ' '
        snowprecip_errfile    =    ' '
        apressure_errfile     =    ' '
        uwind_errfile         =    ' '
        vwind_errfile         =    ' '
        geoid_errfile         =    ' '
        geoid_covariancefile  =    ' '
        bottomdrag_errfile    =    ' '
        edtau_errfile         =    ' '
        kapgm_errfile         =    ' '
        kapredi_errfile       =    ' '
        diffkr_errfile        =    ' '
        do k=1,NUSERCOST
         usercost_datafile(k)  =    ' '
         usercost_errfile(k)   =    ' '
        enddo
        do k=1,NSSHV4COST
         sshv4cost_scalefile(k) =    ' '
         sshv4cost_errfile(k)   =    ' '
         sshv4cost_errfactor(k) =     1. _d 0
        enddo
        tpTimeMaskFile        =    ' '
        ersTimeMaskFile       =    ' '
        gfoTimeMaskFile       =    ' '

        mult_hflux            =      0. _d 0
        mult_sflux            =      0. _d 0
        mult_hfluxmm          =      0. _d 0
        mult_sfluxmm          =      0. _d 0
        mult_tauu             =      0. _d 0
        mult_tauv             =      0. _d 0
        mult_atemp            =      0. _d 0
        mult_aqh              =      0. _d 0
        mult_precip           =      0. _d 0
        mult_swflux           =      0. _d 0
        mult_swdown           =      0. _d 0
        mult_lwflux           =      0. _d 0
        mult_lwdown           =      0. _d 0
        mult_evap             =      0. _d 0
        mult_snowprecip       =      0. _d 0
        mult_apressure        =      0. _d 0
        mult_uwind            =      0. _d 0
        mult_vwind            =      0. _d 0
        mult_hmean            =      0. _d 0
        mult_h                =      1. _d 0
        mult_tp               =      0. _d 0
        mult_ers              =      0. _d 0
        mult_gfo              =      0. _d 0
        mult_temp             =      0. _d 0
        mult_salt             =      0. _d 0
        mult_temp0            =      0. _d 0
        mult_salt0            =      0. _d 0
        mult_etan0            =      0. _d 0
        mult_uvel0            =      0. _d 0
        mult_vvel0            =      0. _d 0
        mult_tmi              =      0. _d 0
        mult_sst              =      0. _d 0
        mult_sss              =      0. _d 0
        mult_bp               =      0. _d 0
        mult_ies              =      0. _d 0
        mult_ctdt             =      0. _d 0
        mult_ctds             =      0. _d 0
        mult_ctdtclim         =      0. _d 0
        mult_ctdsclim         =      0. _d 0
        mult_xbt              =      0. _d 0
        mult_argot            =      0. _d 0
        mult_argos            =      0. _d 0
        mult_drift            =      0. _d 0
        mult_tdrift           =      0. _d 0
        mult_sdrift           =      0. _d 0
        mult_wdrift           =      0. _d 0
        mult_scatx            =      0. _d 0
        mult_scaty            =      0. _d 0
#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
        mult_obcsn            =      0. _d 0
        mult_obcss            =      0. _d 0
        mult_obcsw            =      0. _d 0
        mult_obcse            =      0. _d 0
        mult_obcsvol          =      0. _d 0
        mult_ageos            =      0. _d 0
#endif
        mult_curmtr           =      0. _d 0
        mult_kapgm            =      0. _d 0
        mult_kapredi            =      0. _d 0
        mult_diffkr           =      0. _d 0
        mult_edtau            =      0. _d 0
        mult_bottomdrag       =      0. _d 0
        do k=1,NUSERCOST
        mult_usercost(k)       =      0. _d 0
        enddo
        do k=1,NSSHV4COST
        mult_sshv4cost(k)       =      0. _d 0
        enddo
        mult_smooth_ic        =      0. _d 0
        mult_smooth_bc        =      0. _d 0
        mult_transp           =      0. _d 0
        watemp0               =      0. _d 0
        waqh0                 =      0. _d 0
        wprecip0              =      0. _d 0
        wswflux0              =      0. _d 0
        wswdown0              =      0. _d 0
        wlwflux0              =      0. _d 0
        wlwdown0              =      0. _d 0
        wevap0                =      0. _d 0
        wsnowprecip0          =      0. _d 0
        wapressure0           =      0. _d 0
        wwind0                =      0. _d 0
        wdiffkr0              =      1. _d 0
        wkapgm0               =      1. _d 0
        wkapredi0               =      1. _d 0
        wedtau0               =      1. _d 0
        whflux0               =      0. _d 0
        wsflux0               =      0. _d 0
        wtau0                 =      0. _d 0
        wbottomdrag0          =      1. _d 0
        wmean_atemp           =      0. _d 0
        wmean_aqh             =      0. _d 0
        wmean_precip          =      0. _d 0
        wmean_swflux          =      0. _d 0
        wmean_swdown          =      0. _d 0
        wmean_lwflux          =      0. _d 0
        wmean_lwdown          =      0. _d 0
        wmean_evap            =      0. _d 0
        wmean_snowprecip      =      0. _d 0
        wmean_apressure       =      0. _d 0
        wmean_wind            =      0. _d 0
        wmean_hflux           =      0. _d 0
        wmean_sflux           =      0. _d 0
        wmean_tau             =      0. _d 0
        cost_iprec  = precFloat32
        ecco_output_sterGloH  =      .FALSE.
        ecco_keepTSeriesOutp_open =  .FALSE.

Catn-- retired parameters
        topexmeanfile         =    ' '
        cost_yftype           =   'RL'
        nRetired              =      0

c--     Next, read the data.ecco file.
        WRITE(msgBuf,'(A)') 'ECCO_READPARMS: opening data.ecco'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )

        CALL OPEN_COPY_DATA_FILE(
     I                          'data.ecco', 'ECCO_READPARMS',
     O                          iUnit,
     I                          myThid )

        READ(  iUnit, nml = ecco_cost_nml )

        WRITE(msgBuf,'(A)')
     &       'ECCO_READPARMS: finished reading #1: ecco_cost_nml'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )

C--   Check parameters:
        IF ( cost_yftype.NE.'RL' ) THEN
          STOP 'ABNORMAL END: S/R ECCO_READPARMS: invalid cost_yftype'
        ENDIF
Catn -- retired parameters
      IF ( topexmeanfile .NE. ' ' ) THEN
       nRetired = nRetired + 1
       WRITE(msgBuf,'(A,A)')
     &  'S/R ECCO_READPARMS: "topexmeanfile" ',
     &  'is no longer allowed in file "data.ecco"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'S/R ECCO_READPARMS: set "mdtdatfile" instead'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R ECCO_READPARMS'
      ENDIF
      IF ( nRetired .GT. 0 ) THEN
       WRITE(msgBuf,'(2A)') 'S/R ECCO_READPARMS: ',
     &  'Error reading parameter file "data.ecco"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'some out of date parameters were found in the namelist'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R ECCO_READPARMS'
      ENDIF

      _END_MASTER( myThid )

c     empty pre/post-processing :
c     =========================

      _BEGIN_MASTER( myThid )
      do k2=1,NGENPPROC
       no_preproc(k2)     = ' '
       no_preproc_c(k2)   = ' '
       no_preproc_i(k2)   = 0
       no_preproc_r(k2)   = 0. _d 0
       no_posproc(k2)     = ' '
       no_posproc_c(k2)   = ' '
       no_posproc_i(k2)   = 0
       no_posproc_r(k2)   = 0. _d 0
       clim_preproc(k2)   = ' '
       clim_preproc_i(k2) = 0
       anom_preproc(k2)   = ' '
      enddo
      clim_preproc(1)   = 'clim'
      clim_preproc_i(1) = 12
      anom_preproc(1)   = 'anom'
      _END_MASTER( myThid )

#ifdef ALLOW_GENCOST_CONTRIBUTION

      _BEGIN_MASTER( myThid )

c--   Set default values.
      do k=1,NGENCOST
         using_gencost(k)      = .FALSE.
         gencost_flag(k)       = 0
         gencost_avgperiod(k)  = '     '
         gencost_startdate1(k) = 0
         gencost_startdate2(k) = 0
         gencost_enddate1(k) = 0
         gencost_enddate2(k) = 0
         gencost_datafile(k)   = ' '
         gencost_name(k)       = 'gencost'
         do k2=1,NGENPPROC
          gencost_preproc(k2,k)   = ' '
          gencost_preproc_c(k2,k) = ' '
          gencost_preproc_i(k2,k) = 0
          gencost_preproc_r(k2,k) = 0. _d 0
          gencost_posproc(k2,k)   = ' '
          gencost_posproc_c(k2,k) = ' '
          gencost_posproc_i(k2,k) = 0
          gencost_posproc_r(k2,k) = 0. _d 0
         enddo
         gencost_outputlevel(k)= 0
         gencost_errfile(k)    = ' '
         gencost_itracer(k)    = 1
         gencost_kLev_select(k)= 1
         gencost_mask(k)       = ' '
         gencost_barfile(k)    = ' '
         gencost_spmin(k)      = 0. _d 0
         gencost_spmax(k)      = 0. _d 0
         gencost_spzero(k)     = 9876. _d 0
         gencost_wei1d(k)      = 0. _d 0
         mult_gencost(k)       = 1. _d 0
         gencost_is1d(k)       = .FALSE.
         gencost_is3d(k)       = .FALSE.
         gencost_useDensityMask(k) = .FALSE.
         gencost_refPressure(k) = 0. _d 0
         gencost_sigmaLow(k)    = 0. _d 0
         gencost_sigmaHigh(k)   = 1. _d 3
         gencost_tanhScale(k)   = 1. _d 5
         gencost_pointer3d(k)  = 0
         gencost_msk_is3d(k)   = .FALSE.
         gencost_msk_pointer3d(k)  = 0
c boxmean/horflux masks
         call ecco_zero(gencost_mskCsurf(1-OLx,1-OLy,1,1,k),
     &          1,zeroRL,myThid)
         call ecco_zero(gencost_mskWsurf(1-OLx,1-OLy,1,1,k),
     &          1,zeroRL,myThid)
         call ecco_zero(gencost_mskSsurf(1-OLx,1-OLy,1,1,k),
     &          1,zeroRL,myThid)
         do k2 = 1,Nr
          gencost_mskVertical(k2,k)= 1. _d 0
         enddo
c deprecated:
        gencost_nrecperiod(k) = 0
        gencost_scalefile(k)  = ' '
        gencost_smooth2Ddiffnbt(k) = 0
        gencost_timevaryweight(k)  = .FALSE.
      enddo

#ifdef ALLOW_GENCOST3D
      do k=1,NGENCOST3D
c boxmean/horflux masks
         call ecco_zero(gencost_mskC(1-OLx,1-OLy,1,1,1,k),
     &          Nr,zeroRL,myThid)
         call ecco_zero(gencost_mskW(1-OLx,1-OLy,1,1,1,k),
     &          Nr,zeroRL,myThid)
         call ecco_zero(gencost_mskS(1-OLx,1-OLy,1,1,1,k),
     &          Nr,zeroRL,myThid)
      enddo
#endif

      READ(  iUnit, nml = ecco_gencost_nml )

      WRITE(msgBuf,'(A)')
     &     'ECCO_READPARMS: finished reading #2: ecco_gencost_nml'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      gencost_k3d=1
      gencost_msk_k3d=1

      do k=1,NGENCOST

c pre-specified generic cost terms using otherwise created barfiles
         if ( (gencost_name(k).EQ.'sshv4-mdt').OR.
     &        (gencost_name(k).EQ.'sshv4-tp').OR.
     &        (gencost_name(k).EQ.'sshv4-ers').OR.
     &        (gencost_name(k).EQ.'sshv4-gfo').OR.
     &        (gencost_name(k).EQ.'sshv4-lsc').OR.
     &        (gencost_name(k).EQ.'sshv4-gmsl').OR.
     &        (gencost_name(k).EQ.'bpv4-grace').OR.
     &        (gencost_name(k).EQ.'sstv4-amsre').OR.
     &        (gencost_name(k).EQ.'sstv4-amsre-lsc') ) then
           gencost_flag(k)=-1
           using_gencost(k)=.TRUE.

catn move boxmean outside -1 to have its own check
catn preserve -2 for potential separate group above into cost
catn with and without requirements of datafile or errfile
         elseif ( (gencost_barfile(k)(1:9).EQ.'m_boxmean').OR.
     &            (gencost_barfile(k)(1:9).EQ.'m_horflux') ) then
           gencost_flag(k)=-3
           using_gencost(k)=.TRUE.
           il=ilnblnk(gencost_name(k))
           WRITE(msgBuf,'(A,I3,3A,L5)') 'gencost k_boxmean/horflux=',
     &     k,' ',gencost_name(k)(1:il),': ',using_gencost(k)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )

           if ( gencost_msk_is3d(k) ) then
             gencost_msk_pointer3d(k)=gencost_msk_k3d
             gencost_msk_k3d=gencost_msk_k3d+1
           endif

catn add cost global transport using gencost framework
         elseif ( gencost_name(k)(1:6).EQ.'transp' ) then
           gencost_flag(k)=-4
           using_gencost(k)=.TRUE.
           il=ilnblnk(gencost_name(k))
           WRITE(msgBuf,'(A,i3,3A,L5)') 'gencost k_transp=',
     &     k,' ',gencost_name(k)(1:il),': ',using_gencost(k)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )

cts moc cost function using gencost framework
         elseif ( gencost_name(k)(1:3).EQ.'moc' ) then
           gencost_flag(k)=-5
           using_gencost(k)=.TRUE.
           il=ilnblnk(gencost_name(k))
           WRITE(msgBuf,'(A,i2,2A,L5)') 'gencost k_moc=',
     &     k,gencost_name(k)(1:il),': ',using_gencost(k)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )

c pre-specified generic cost terms that create their own barfile
         elseif (
     &        (gencost_name(k).EQ.'siv4-conc').OR.
     &        (gencost_name(k).EQ.'siv4-deconc').OR.
     &        (gencost_name(k).EQ.'siv4-exconc') ) then
           gencost_flag(k)=2
           using_gencost(k)=.TRUE.
c fully interactive generic cost terms
         elseif ( gencost_datafile(k) .NE. ' ' ) then
           gencost_flag(k)=1
           using_gencost(k)=.TRUE.
         endif

c identified three dimensional variables
         if ( (gencost_barfile(k)(1:7).EQ.'m_theta').OR.
     &        (gencost_barfile(k)(1:6).EQ.'m_salt').OR.
     &        (gencost_barfile(k)(1:8).EQ.'m_diffkr').OR.
     &        (gencost_barfile(k)(1:7).EQ.'m_kapgm').OR.
     &        (gencost_barfile(k)(1:9).EQ.'m_kapredi').OR.
     &        (gencost_barfile(k)(1:7).EQ.'m_trVol').OR.
     &        (gencost_barfile(k)(1:8).EQ.'m_trHeat').OR.
     &        (gencost_barfile(k)(1:8).EQ.'m_trSalt')
     &       )
     &        gencost_is3d(k)=.TRUE.

c assign a 3D storage array, then increment 3D variables counter
         if ( gencost_is3d(k) ) then
           gencost_pointer3d(k)=gencost_k3d
           gencost_k3d=gencost_k3d+1
         endif

      enddo

C--   load masks if needed:
      do k=1,NGENCOST
           kk=gencost_msk_pointer3d(k)
           if ( ( gencost_mask(k) .NE. ' ' ).AND.
     &          (gencost_flag(k).EQ.-3 .or.
     &           gencost_flag(k).eq.-4 .or.
     &           gencost_flag(k).eq.-5    ) ) then
c
            il = ilnblnk(gencost_mask(k))
            write(tempfile(1:128),'(2A)') gencost_mask(k)(1:il),'C'
            inquire( file=tempfile(1:il+1), exist=exst )
            if (exst.AND.(.NOT.gencost_msk_is3d(k))) then
             CALL READ_REC_3D_RL( tempfile, cost_iprec, 1,
     &            gencost_mskCsurf(1-OLx,1-OLy,1,1,k), 1, 1, myThid )
#ifdef ALLOW_GENCOST3D
            elseif (exst.AND.(gencost_msk_pointer3d(k).LE.
     &                        NGENCOST3D)) then
             CALL READ_REC_3D_RL( tempfile, cost_iprec, Nr,
     &            gencost_mskC(1-OLx,1-OLy,1,1,1,kk), 1, 1, myThid )
#endif
            endif
c
            il = ilnblnk(gencost_mask(k))
            write(tempfile(1:128),'(2A)') gencost_mask(k)(1:il),'W'
            inquire( file=tempfile(1:il+1), exist=exst )
            if (exst.AND.(.NOT.gencost_msk_is3d(k))) then
             CALL READ_REC_3D_RL( tempfile, cost_iprec, 1,
     &            gencost_mskWsurf(1-OLx,1-OLy,1,1,k), 1, 1, myThid )
#ifdef ALLOW_GENCOST3D
            elseif (exst.AND.(gencost_msk_pointer3d(k).LE.
     &                        NGENCOST3D)) then
             CALL READ_REC_3D_RL( tempfile, cost_iprec, Nr,
     &            gencost_mskW(1-OLx,1-OLy,1,1,1,kk), 1, 1, myThid )
#endif
            endif
c
            il = ilnblnk(gencost_mask(k))
            write(tempfile(1:128),'(2A)') gencost_mask(k)(1:il),'S'
            inquire( file=tempfile(1:il+1), exist=exst )
            if (exst.AND.(.NOT.gencost_msk_is3d(k))) then
             CALL READ_REC_3D_RL( tempfile, cost_iprec, 1,
     &            gencost_mskSsurf(1-OLx,1-OLy,1,1,k), 1, 1, myThid )
#ifdef ALLOW_GENCOST3D
            elseif (exst.AND.(gencost_msk_pointer3d(k).LE.
     &                        NGENCOST3D)) then
             CALL READ_REC_3D_RL( tempfile, cost_iprec, Nr,
     &            gencost_mskS(1-OLx,1-OLy,1,1,1,kk), 1, 1, myThid )
#endif
            endif
c
            il = ilnblnk(gencost_mask(k))
            write(tempfile(1:128),'(2A)') gencost_mask(k)(1:il),'K'
            inquire( file=tempfile(1:il+1), exist=exst )
            if (exst) then
             ioUnit = 0
             CALL MDS_READVEC_LOC( tempfile, cost_iprec, ioUnit, 'RL',
     &        Nr, gencost_mskVertical(1,k), dummyRS, 0, 0, 1, myThid )
            endif
c
           endif!errfile
      enddo

C--   Check parameters:
      do k=1,NGENCOST

cgf ensure backward compatibility:
        if (gencost_barfile(k).NE.' ') then
         if (gencost_barfile(k)(1:2).NE.'m_') then
          il=ilnblnk(gencost_barfile(k))
          WRITE(msgBuf,'(A,A)') 'm_',gencost_barfile(k)(1:il)
          gencost_barfile(k)=msgBuf(1:il+2)
         endif
         if (gencost_barfile(k)(1:8).EQ.'m_tauZon') then
          il=ilnblnk(gencost_barfile(k))
          WRITE(msgBuf,'(A,A)') 'm_ustress',gencost_barfile(k)(9:il)
          gencost_barfile(k)=msgBuf(1:il+1)
         endif
         if (gencost_barfile(k)(1:8).EQ.'m_tauMer') then
          il=ilnblnk(gencost_barfile(k))
          WRITE(msgBuf,'(A,A)') 'm_vstress',gencost_barfile(k)(9:il)
          gencost_barfile(k)=msgBuf(1:il+1)
         endif
        endif

catn separate out blocks of gencost_flag so that each block is checked
        if ( using_gencost(k)) then
         if ( gencost_flag(k).GE.1 ) then

          if ( gencost_name(k) .EQ. 'gencost' )
     &        gencost_name(k) = gencost_datafile(k)

          if ( gencost_avgperiod(k) .NE. 'day  ' .AND.
     &        gencost_avgperiod(k) .NE. 'DAY  ' .AND.
     &        gencost_avgperiod(k) .NE. 'month' .AND.
     &        gencost_avgperiod(k) .NE. 'MONTH' .AND.
     &        gencost_avgperiod(k) .NE. 'step' .AND.
     &        gencost_avgperiod(k) .NE. 'STEP' .AND.
     &        gencost_avgperiod(k) .NE. 'const' .AND.
     &        gencost_avgperiod(k) .NE. 'CONST' .AND.
     &        gencost_avgperiod(k) .NE. 'year ' .AND.
     &        gencost_avgperiod(k) .NE. 'YEAR ' ) then

            il=ilnblnk(gencost_name(k))
            WRITE(msgBuf,'(A,I2,2A)')
     &          'ERROR in ECCO_READPARMS: for gencost',k,
     &          '  ',gencost_name(k)(1:il)
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            STOP
     &           'ECCO_READPARMS: gencost_avgperiod not properly set'
          endif

          if ( gencost_spmin(k) .EQ. 0. _d 0 .AND.
     &        gencost_spmax(k) .EQ. 0. _d 0 ) then
            il=ilnblnk(gencost_name(k))
            WRITE(msgBuf,'(A,I2,2A)')
     &          'ERROR in ECCO_READPARMS: for gencost',k,
     &          '  ',gencost_name(k)(1:il)
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            STOP
     &       'ECCO_READPARMS: gencost_spmin, gencost_spmax not set'
          endif

          if ( gencost_spzero(k) .EQ. 9876. _d 0 ) then
            il=ilnblnk(gencost_name(k))
            WRITE(msgBuf,'(A,I2,2A)')
     &          'ERROR in ECCO_READPARMS: for gencost',k,
     &          '  ',gencost_name(k)(1:il)
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            STOP
     &       'ECCO_READPARMS: gencost_spzero not set'
          endif

catn -- checking for -1 flag
         elseif( gencost_flag(k) .eq. -1 ) then
catn -- checking boxmean and transp
         elseif( gencost_flag(k).lt.-1) then
            if ( gencost_avgperiod(k) .NE. 'day  ' .AND.
     &           gencost_avgperiod(k) .NE. 'DAY  ' .AND.
     &           gencost_avgperiod(k) .NE. 'month' .AND.
     &           gencost_avgperiod(k) .NE. 'MONTH' .AND.
     &           gencost_avgperiod(k) .NE. 'step' .AND.
     &           gencost_avgperiod(k) .NE. 'STEP' .AND.
     &           gencost_avgperiod(k) .NE. 'const' .AND.
     &           gencost_avgperiod(k) .NE. 'CONST' .AND.
     &           gencost_avgperiod(k) .NE. 'year ' .AND.
     &           gencost_avgperiod(k) .NE. 'YEAR ' ) then

              il=ilnblnk(gencost_name(k))
              WRITE(msgBuf,'(A,I2,2A)')
     &          'ERROR in ECCO_READPARMS: for gencost',k,
     &          '  ',gencost_name(k)(1:il)
              CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                            SQUEEZE_RIGHT, myThid )
            STOP
     &           'ECCO_READPARMS: gencost_avgperiod not properly set'
            endif
         endif !gencost_flag
        endif !using_gencost

      end do

      _END_MASTER( myThid )

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      _BEGIN_MASTER( myThid )

#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

      _END_MASTER( myThid )

      WRITE(msgBuf,'(A)')
     &     'ECCO_READPARMS: done'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      _BARRIER

      RETURN
      END
