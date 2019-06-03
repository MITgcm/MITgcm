c     ==================================================================
c     HEADER AVERAGES
c     ==================================================================
c
c     o Header for averaged temperature, salinity, and surface pressure
c       fields and counters associated with the averaging.
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     ==================================================================
c     HEADER AVERAGES
c     ==================================================================

#include "ecco.h"

#ifdef ECCO_CTRL_DEPRECATED

c     Number of days: (hard-coded to set up some vector dimensions
c     =============================
c     22 years: 8050
      INTEGER maxNumDays
      PARAMETER ( maxNumDays = 8050 )

c     Number of levels
c     ================
      common /ecco_cost_i/
     &                nnztbar,
     &                nnzsbar
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
     &               ,nnzsigmaRbar
#endif
      integer nnztbar
      integer nnzsbar
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      integer nnzsigmaRbar
#endif

c     Number of sshv4cost Cost terms:
c     =============================
      INTEGER NSSHV4COST
      PARAMETER ( NSSHV4COST=5 )

c     Averaged Fields:
c     ================
c
c     tbar  - contains the averaged temperature field after the call
c             to subroutine POST_MONTHLY. Before, it accumulates the
c             intantaneous temperatures.
c     sbar  - contains the averaged salinity field after the call
c             to subroutine POST_MONTHLY. Before, it accumulates the
c             intantaneous salinities.
c     sigmaRbar - contains the averaged sigmaR field after the call
c             to subroutine POST_MONTHLY. Before, it accumulates the
c             intantaneous sigmaR.
c     psbar - contains the averaged surface pressure field after the call
c             to subroutine POST_DAILY. Before, it accumulates the
c             intantaneous surface pressure field.
c     ubar  - contains the averaged zonal velocity component for the
c             whole integration period. Before, it accumulates the
c             intantaneous field.
c     vbar  - contains the averaged zonal velocity component for the
c             whole integration period. Before, it accumulates the
c             intantaneous field.
c     tauxbar  - contains the averaged zonal velocity component for the
c             whole integration period. Before, it accumulates the
c             intantaneous field.
c     tauybar  - contains the averaged zonal velocity component for the
c             whole integration period. Before, it accumulates the
c             intantaneous field.
c     hfluxmeanbar  - contains the averaged zonal velocity component for the
c             whole integration period. Before, it accumulates the
c             intantaneous field.
c     sfluxmeanbar  - contains the averaged zonal velocity component for the
c             whole integration period. Before, it accumulates the
c             intantaneous field.

      common /averages_r/
     &                    tbar,
     &                    sbar,
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
     &                    sigmaRbar,
#endif
     &                    sstbar,
     &                    psbar,
     &                    bpbar,
     &                    iestaubar,
     &                    ubar,
     &                    vbar,
     &                    wbar,
     &                    tauxbar,
     &                    tauybar,
     &                    hfluxmeanbar,
     &                    sfluxmeanbar,
     &                    Slmean,
     &                    Tlmean,
     &                    wlmean,
     &                    Sfmean,
     &                    Tfmean,
     &                    sbar_gen,
     &                    tbar_gen,
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
     &                    sigmaRbar_gen,
#endif
     &                    wfmean

#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      _RL sigmaRbar      (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL sigmaRbar_gen  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif
#if (defined (ALLOW_THETA_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDT_COST_CONTRIBUTION) || \
     defined (ALLOW_XBT_COST_CONTRIBUTION) || \
     defined (ALLOW_DRIFT_COST_CONTRIBUTION) || \
     defined (ALLOW_COST_TRANSPORT) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
      _RL tbar  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#else
#ifdef ALLOW_SST_COST_CONTRIBUTION
      _RL tbar  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
#else
      _RL tbar
#endif
#endif
#ifdef GENERIC_BAR_MONTH
      _RL tbar_gen  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL sbar_gen  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#else
      _RL tbar_gen
      _RL sbar_gen
#endif

#ifdef ALLOW_DAILYSST_COST_CONTRIBUTION
cph#ifdef ALLOW_SEAICE_COST_AREASST
      _RL sstbar  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
#else
      _RL sstbar
#endif

#if (defined (ALLOW_SALT_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDS_COST_CONTRIBUTION) || \
     defined (ALLOW_DRIFT_COST_CONTRIBUTION) || \
     defined (ALLOW_COST_TRANSPORT) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
      _RL sbar  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#else
#ifdef ALLOW_SSS_COST_CONTRIBUTION
      _RL sbar  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
#else
      _RL sbar
#endif
#endif

#ifdef ALLOW_SSH_COST_CONTRIBUTION
      _RL psbar  (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)
#else
      _RL psbar
#endif

#ifdef ALLOW_BP_COST_CONTRIBUTION
      _RL bpbar  (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)
#else
      _RL bpbar
#endif

#ifdef ALLOW_IESTAU_COST_CONTRIBUTION
      _RL iestaubar  (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)
#else
      _RL iestaubar
#endif

#if (defined (ALLOW_DRIFTER_COST_CONTRIBUTION) || \
     defined (ALLOW_COST_TRANSPORT) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
      _RL ubar  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL vbar  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#else
      _RL ubar
      _RL vbar
#endif

#ifdef ALLOW_DRIFTW_COST_CONTRIBUTION
      _RL wbar  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#else
      _RL wbar
#endif

#ifdef ALLOW_DRIFT_COST_CONTRIBUTION
      _RL   Tlmean(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL   Slmean(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL   Tfmean(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL   Sfmean(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#else
      _RL   Tlmean
      _RL   Slmean
      _RL   Tfmean
      _RL   Sfmean
#endif

#ifdef ALLOW_DRIFTW_COST_CONTRIBUTION
      _RL   wlmean(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL   wfmean(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#else
      _RL   wlmean
      _RL   wfmean
#endif

#if (defined (ALLOW_SCAT_COST_CONTRIBUTION) || \
     defined (ALLOW_DAILYSCAT_COST_CONTRIBUTION) )
      _RL tauxbar  (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)
      _RL tauybar  (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)
#else
      _RL tauxbar
      _RL tauybar
#endif

#ifdef ALLOW_MEAN_HFLUX_COST_CONTRIBUTION
      _RL hfluxmeanbar  (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)
#else
      _RL hfluxmeanbar
#endif

#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
      _RL sfluxmeanbar  (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)
#else
      _RL sfluxmeanbar
#endif


      common /averages_c/
     &                    tbarfile,
     &                    sbarfile,
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
     &                    sigmaRbarfile,
#endif
     &                    sstbarfile,
     &                    psbarfile,
     &                    bpbarfile,
     &                    iestaubarfile,
     &                    ubarfile,
     &                    vbarfile,
     &                    wbarfile,
     &                    tauxbarfile,
     &                    tauybarfile,
     &                    hfluxmeanbarfile,
     &                    sfluxmeanbarfile,
     &                    costTranspDataFile
      character*(MAX_LEN_FNAM) tbarfile
      character*(MAX_LEN_FNAM) sbarfile
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      character*(MAX_LEN_FNAM) sigmaRbarfile
#endif
      character*(MAX_LEN_FNAM) sstbarfile
      character*(MAX_LEN_FNAM) psbarfile
      character*(MAX_LEN_FNAM) bpbarfile
      character*(MAX_LEN_FNAM) iestaubarfile
      character*(MAX_LEN_FNAM) ubarfile
      character*(MAX_LEN_FNAM) vbarfile
      character*(MAX_LEN_FNAM) wbarfile
      character*(MAX_LEN_FNAM) tauxbarfile
      character*(MAX_LEN_FNAM) tauybarfile
      character*(MAX_LEN_FNAM) hfluxmeanbarfile
      character*(MAX_LEN_FNAM) sfluxmeanbarfile
      character*(MAX_LEN_FNAM) costTranspDataFile

#ifdef ALLOW_TRANSPORT_COST_CONTRIBUTION
      common /averages_transp_r/
     &                     transpbar
     &                   , transpobs
     &                   , wtransp
      _RL transpbar(maxNumDays,nsx,nsy)
      _RL transpobs(maxNumDays)
      _RL wtransp(maxNumDays)
#endif

c     ==================================================================
c     END OF HEADER AVERAGES
c     ==================================================================



c     ==================================================================
c     HEADER COST
c     ==================================================================
c
c     o Header for model-data comparison.
c
c     The individual cost function contributions are multiplied by
c     factors mult_"var" which allow to switch off these contributions
c     without removing them in the adjoint code. This is useful for
c     doing tests with the adjoint and perhaps useful in assimilation
c     experiments where individual contributions are successively
c     switched on. For future applications it would be better to place
c     the initialisation of the multipliers somewhere else, for example
c     in a namelist, which is read in at the start of the model.
c
c     started: Christian Eckert eckert@mit.edu  24-Feb-1999
c
c     changed: Christian Eckert eckert@mit.edu
c
c
c     ==================================================================
c     HEADER COST
c     ==================================================================


c     The cost function, its contributions, and multipliers:
c     ======================================================
c
c     fc         - Final cost function.
c     objf_hflux    - Heat flux contribution.
c     objf_sflux    - Salt flux contribution.
c     objf_tauu  - Zonal wind stress contribution.
c     objf_tauv  - Meridional wind stress contribution.
c     objf_hfluxm    - time-mean Heat flux contribution.
c     objf_sfluxm    - time-mean Salt flux contribution.
c     objf_tauum  - time-mean Zonal wind stress contribution.
c     objf_tauvm  - time-mean Meridional wind stress contribution.
c     objf_hfluxmm    - Global time-mean Heat flux contribution.
c     objf_sfluxmm    - Global time-mean Salt flux contribution.
c     objf_hmean - Mean sea surface height contribution.
c     objf_h     - Residual sea surface height contribution.
c     objf_tp    - Residual sea surface height contribution from T/P
c     objf_ers   - Residual sea surface height contribution from T/P
c     objf_gfo   - Residual sea surface height contribution from T/P
c     objf_temp  - Temperature contribution.
c     objf_salt  - Salinity contribution.
c     objf_sigmaR  - sigmaR contribution.
c     objf_temp0 - Initial conditions Temperature contribution.
c     objf_salt0 - Initial conditions Salinity contribution.
c     objf_sst   - Sea surface temperature contribution.
c     objf_tmi   - Sea surface temperature contribution.
c     objf_sss   - Sea surface salinity contribution.
c     objf_ctdt  - Temperature measurements from Woce CTD
c     objf_ctds  - Salinity measurements from Woce CTD
c     objf_ctdtclim - Temperature measurements from Woce CTD without timetag
c     objf_ctdsclim - Salinity measurements from Woce CTD without timetag
c     objf_xbt   - XBT temperature data
c     objf_argot - ARGO temperature profiles
c     objf_argos - ARGO salt profiles
c     objf_usercost - user defined cost contribution
c     objf_scatxm - time-mean zonal SCAT  contribution
c     objf_scatym - time-mean meridional SCAT  contribution
c     objf_scatx  - zonal SCAT  contribution
c     objf_scaty  - meridional SCAT  contribution
c     objf_kapgm  - kappa GM contribution
c     objf_kapredi  - kappa REDI contribution
c     objf_diffkr - diffusion contribution
c     objf_theta_ini_fin - final vs. initial theta misfit
c     objf_salt_ini_fin  - final vs. initial salt misfit
c     objf_eddytau - eddy streamfunction contribution
c     objf_bottomdrag - bottom drag contribution
c
c     mult_"var" - multipliers for the individual cost
c                  function contributions.

      common /ecco_cost_objf/
     &     objf_hflux, objf_hfluxm, objf_hfluxmm, objf_hfluxsmoo,
     &     objf_sflux, objf_sfluxm, objf_sfluxmm, objf_sfluxsmoo,
     &     objf_tauu,  objf_tauum,  objf_tauusmoo,
     &     objf_tauv,  objf_tauvm,  objf_tauvsmoo,
     &     objf_hmean,
     &     objf_h, objf_tp, objf_ers, objf_gfo,
     &     objf_sshv4cost,
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
     &     objf_sigmaR,
#endif
     &     objf_temp,      objf_salt,
     &     objf_temp0,     objf_salt0,
     &     objf_temp0smoo, objf_salt0smoo,
     &     objf_etan0, objf_uvel0, objf_vvel0,
     &     objf_sst, objf_tmi, objf_sss,
     &     objf_bp, objf_ies,
     &     objf_usercost,
     &     objf_ctdt,      objf_ctds,
     &     objf_ctdtclim,  objf_ctdsclim,
     &     objf_xbt, objf_argot,     objf_argos,
     &     objf_drift, objf_tdrift, objf_sdrift, objf_wdrift,
     &     objf_scatx, objf_scaty,  objf_scatxm, objf_scatym,
     &     objf_atemp,      objf_atempm,      objf_atempsmoo,
     &     objf_aqh,        objf_aqhm,        objf_aqhsmoo,
     &     objf_precip,     objf_precipm,     objf_precipsmoo,
     &     objf_swflux,     objf_swfluxm,     objf_swfluxsmoo,
     &     objf_swdown,     objf_swdownm,     objf_swdownsmoo,
     &     objf_snowprecip, objf_snowprecipm, objf_snowprecipsmoo,
     &     objf_lwflux,     objf_lwfluxm,     objf_lwfluxsmoo,
     &     objf_lwdown,     objf_lwdownm,     objf_lwdownsmoo,
     &     objf_evap,       objf_evapm,       objf_evapsmoo,
     &     objf_apressure,  objf_apressurem,  objf_apressuresmoo,
     &     objf_runoff,     objf_runoffm,     objf_runoffsmoo,
     &     objf_uwind,      objf_uwindm,      objf_uwindsmoo,
     &     objf_vwind,      objf_vwindm,      objf_vwindsmoo,
     &     objf_curmtr,
     &     objf_kapgm,
     &     objf_kapredi,
     &     objf_diffkr,
     &     objf_theta_ini_fin, objf_salt_ini_fin,
     &     objf_eddytau,
     &     objf_bottomdrag,
     &     objf_transp

      _RL  objf_hflux  (nsx,nsy)
      _RL  objf_hfluxm (nsx,nsy)
      _RL  objf_hfluxmm
      _RL  objf_hfluxsmoo  (nsx,nsy)
      _RL  objf_sflux  (nsx,nsy)
      _RL  objf_sfluxm (nsx,nsy)
      _RL  objf_sfluxmm
      _RL  objf_sfluxsmoo  (nsx,nsy)
      _RL  objf_tauu   (nsx,nsy)
      _RL  objf_tauum  (nsx,nsy)
      _RL  objf_tauusmoo   (nsx,nsy)
      _RL  objf_tauv   (nsx,nsy)
      _RL  objf_tauvm  (nsx,nsy)
      _RL  objf_tauvsmoo   (nsx,nsy)
      _RL  objf_hmean
      _RL  objf_h    (nsx,nsy)
      _RL  objf_tp   (nsx,nsy)
      _RL  objf_ers  (nsx,nsy)
      _RL  objf_gfo  (nsx,nsy)
      _RL  objf_sshv4cost(NSSHV4COST,nsx,nsy)
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      _RL  objf_sigmaR(nsx,nsy)
#endif
      _RL  objf_temp (nsx,nsy)
      _RL  objf_salt (nsx,nsy)
      _RL  objf_temp0(nsx,nsy)
      _RL  objf_salt0(nsx,nsy)
      _RL  objf_temp0smoo(nsx,nsy)
      _RL  objf_salt0smoo(nsx,nsy)
      _RL  objf_etan0(nsx,nsy)
      _RL  objf_uvel0(nsx,nsy)
      _RL  objf_vvel0(nsx,nsy)
      _RL  objf_sst  (nsx,nsy)
      _RL  objf_tmi  (nsx,nsy)
      _RL  objf_sss  (nsx,nsy)
      _RL  objf_bp   (nsx,nsy)
      _RL  objf_ies  (nsx,nsy)
      _RL  objf_ctdt (nsx,nsy)
      _RL  objf_ctds (nsx,nsy)
      _RL  objf_ctdtclim (nsx,nsy)
      _RL  objf_ctdsclim (nsx,nsy)
      _RL  objf_xbt  (nsx,nsy)
      _RL  objf_argot(nsx,nsy)
      _RL  objf_usercost(NUSERCOST,nsx,nsy)
      _RL  objf_argos(nsx,nsy)
      _RL  objf_drift(nsx,nsy)
      _RL  objf_tdrift(nsx,nsy)
      _RL  objf_sdrift(nsx,nsy)
      _RL  objf_wdrift(nsx,nsy)
      _RL  objf_scatx(nsx,nsy)
      _RL  objf_scaty(nsx,nsy)
      _RL  objf_scatxm(nsx,nsy)
      _RL  objf_scatym(nsx,nsy)
      _RL  objf_atemp(nsx,nsy)
      _RL  objf_aqh  (nsx,nsy)
      _RL  objf_precip(nsx,nsy)
      _RL  objf_swflux(nsx,nsy)
      _RL  objf_swdown(nsx,nsy)
      _RL  objf_snowprecip(nsx,nsy)
      _RL  objf_lwflux(nsx,nsy)
      _RL  objf_lwdown(nsx,nsy)
      _RL  objf_evap(nsx,nsy)
      _RL  objf_apressure(nsx,nsy)
      _RL  objf_runoff(nsx,nsy)
      _RL  objf_uwind(nsx,nsy)
      _RL  objf_vwind(nsx,nsy)
      _RL  objf_atempm(nsx,nsy)
      _RL  objf_aqhm  (nsx,nsy)
      _RL  objf_precipm(nsx,nsy)
      _RL  objf_swfluxm(nsx,nsy)
      _RL  objf_swdownm(nsx,nsy)
      _RL  objf_snowprecipm(nsx,nsy)
      _RL  objf_lwfluxm(nsx,nsy)
      _RL  objf_lwdownm(nsx,nsy)
      _RL  objf_evapm(nsx,nsy)
      _RL  objf_apressurem(nsx,nsy)
      _RL  objf_runoffm(nsx,nsy)
      _RL  objf_uwindm(nsx,nsy)
      _RL  objf_vwindm(nsx,nsy)
      _RL  objf_atempsmoo(nsx,nsy)
      _RL  objf_aqhsmoo  (nsx,nsy)
      _RL  objf_precipsmoo(nsx,nsy)
      _RL  objf_swfluxsmoo(nsx,nsy)
      _RL  objf_swdownsmoo(nsx,nsy)
      _RL  objf_snowprecipsmoo(nsx,nsy)
      _RL  objf_lwfluxsmoo(nsx,nsy)
      _RL  objf_lwdownsmoo(nsx,nsy)
      _RL  objf_evapsmoo(nsx,nsy)
      _RL  objf_apressuresmoo(nsx,nsy)
      _RL  objf_runoffsmoo(nsx,nsy)
      _RL  objf_uwindsmoo(nsx,nsy)
      _RL  objf_vwindsmoo(nsx,nsy)
      _RL  objf_curmtr(nsx,nsy)
      _RL  objf_kapgm(nsx,nsy)
      _RL  objf_kapredi(nsx,nsy)
      _RL  objf_diffkr(nsx,nsy)
      _RL  objf_theta_ini_fin(nsx,nsy)
      _RL  objf_salt_ini_fin(nsx,nsy)
      _RL  objf_eddytau(nsx,nsy)
      _RL  objf_bottomdrag(nsx,nsy)
      _RL  objf_transp

      common /ecco_cost_num/
     &                num_hflux,
     &                num_hfluxm,
     &                num_hfluxmm,
     &                num_sflux,
     &                num_sfluxm,
     &                num_sfluxmm,
     &                num_tauu,
     &                num_tauum,
     &                num_tauv,
     &                num_tauvm,
     &                num_hmean,
     &                num_h,
     &                num_tp,
     &                num_ers,
     &                num_gfo,
     &                num_sshv4cost,
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
     &                num_sigmaR,
#endif
     &                num_temp,
     &                num_salt,
     &                num_temp0,
     &                num_salt0,
     &                num_etan0,
     &                num_uvel0,
     &                num_vvel0,
     &                num_sst,
     &                num_tmi,
     &                num_sss,
     &                num_bp,
     &                num_ies,
     &                num_ctdt,
     &                num_ctds,
     &                num_ctdtclim,
     &                num_ctdsclim,
     &                num_xbt,
     &                num_argot,
     &                num_argos,
     &                num_usercost,
     &                num_drift,
     &                num_tdrift,
     &                num_sdrift,
     &                num_wdrift,
     &                num_scatx,
     &                num_scaty,
     &                num_scatxm,
     &                num_scatym,
     &                num_atemp,
     &                num_aqh,
     &                num_precip,
     &                num_swflux,
     &                num_swdown,
     &                num_snowprecip,
     &                num_lwflux,
     &                num_lwdown,
     &                num_evap,
     &                num_apressure,
     &                num_runoff,
     &                num_uwind,
     &                num_vwind,
     &                num_atempm,
     &                num_aqhm,
     &                num_precipm,
     &                num_swfluxm,
     &                num_swdownm,
     &                num_snowprecipm,
     &                num_lwfluxm,
     &                num_lwdownm,
     &                num_evapm,
     &                num_apressurem,
     &                num_runoffm,
     &                num_uwindm,
     &                num_vwindm,
     &                num_curmtr,
     &                num_kapgm,
     &                num_kapredi,
     &                num_diffkr,
     &                num_theta_ini_fin,
     &                num_salt_ini_fin,
     &                num_eddytau,
     &                num_bottomdrag,
     &                num_transp

      _RL  num_hflux  (nsx,nsy)
      _RL  num_hfluxm (nsx,nsy)
      _RL  num_hfluxmm(nsx,nsy)
      _RL  num_sflux  (nsx,nsy)
      _RL  num_sfluxm (nsx,nsy)
      _RL  num_sfluxmm(nsx,nsy)
      _RL  num_tauu   (nsx,nsy)
      _RL  num_tauum  (nsx,nsy)
      _RL  num_tauv   (nsx,nsy)
      _RL  num_tauvm  (nsx,nsy)
      _RL  num_hmean
      _RL  num_h    (nsx,nsy)
      _RL  num_tp   (nsx,nsy)
      _RL  num_ers  (nsx,nsy)
      _RL  num_gfo  (nsx,nsy)
      _RL  num_sshv4cost(NSSHV4COST,nsx,nsy)
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      _RL  num_sigmaR (nsx,nsy)
#endif
      _RL  num_temp (nsx,nsy)
      _RL  num_salt (nsx,nsy)
      _RL  num_temp0(nsx,nsy)
      _RL  num_salt0(nsx,nsy)
      _RL  num_etan0(nsx,nsy) 
      _RL  num_uvel0(nsx,nsy)
      _RL  num_vvel0(nsx,nsy)
      _RL  num_sst  (nsx,nsy)
      _RL  num_tmi  (nsx,nsy)
      _RL  num_sss  (nsx,nsy)
      _RL  num_bp   (nsx,nsy)
      _RL  num_ies  (nsx,nsy)
      _RL  num_ctdt (nsx,nsy)
      _RL  num_ctds (nsx,nsy)
      _RL  num_ctdtclim (nsx,nsy)
      _RL  num_ctdsclim (nsx,nsy)
      _RL  num_xbt  (nsx,nsy)
      _RL  num_argot(nsx,nsy)
      _RL  num_argos(nsx,nsy)
      _RL  num_usercost(NUSERCOST,nsx,nsy)
      _RL  num_drift(nsx,nsy)
      _RL  num_tdrift(nsx,nsy)
      _RL  num_sdrift(nsx,nsy)
      _RL  num_wdrift(nsx,nsy)
      _RL  num_scatx(nsx,nsy)
      _RL  num_scaty(nsx,nsy)
      _RL  num_scatxm(nsx,nsy)
      _RL  num_scatym(nsx,nsy)
      _RL  num_atemp(nsx,nsy)
      _RL  num_aqh  (nsx,nsy)
      _RL  num_precip(nsx,nsy)
      _RL  num_swflux(nsx,nsy)
      _RL  num_swdown(nsx,nsy)
      _RL  num_snowprecip(nsx,nsy)
      _RL  num_lwflux(nsx,nsy)
      _RL  num_lwdown(nsx,nsy)
      _RL  num_evap(nsx,nsy)
      _RL  num_apressure(nsx,nsy)
      _RL  num_runoff(nsx,nsy)
      _RL  num_uwind(nsx,nsy)
      _RL  num_vwind(nsx,nsy)
      _RL  num_atempm(nsx,nsy)
      _RL  num_aqhm  (nsx,nsy)
      _RL  num_precipm(nsx,nsy)
      _RL  num_swfluxm(nsx,nsy)
      _RL  num_swdownm(nsx,nsy)
      _RL  num_snowprecipm(nsx,nsy)
      _RL  num_lwfluxm(nsx,nsy)
      _RL  num_lwdownm(nsx,nsy)
      _RL  num_evapm(nsx,nsy)
      _RL  num_apressurem(nsx,nsy)
      _RL  num_runoffm(nsx,nsy)
      _RL  num_uwindm(nsx,nsy)
      _RL  num_vwindm(nsx,nsy)
      _RL  num_curmtr(nsx,nsy)
      _RL  num_kapgm(nsx,nsy)
      _RL  num_kapredi(nsx,nsy)
      _RL  num_diffkr(nsx,nsy)
      _RL  num_theta_ini_fin(nsx,nsy)
      _RL  num_salt_ini_fin(nsx,nsy)
      _RL  num_eddytau(nsx,nsy)
      _RL  num_bottomdrag(nsx,nsy)
      _RL  num_transp

      common /ecco_cost_aux_r/
     &                    mult_hflux,
     &                    mult_sflux,
     &                    mult_hfluxmm,
     &                    mult_sfluxmm,
     &                    mult_tauu,
     &                    mult_tauv,
     &                    mult_hmean,
     &                    mult_h,
     &                    mult_tp,
     &                    mult_ers,
     &                    mult_gfo,
     &                    mult_sshv4cost,
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
     &                    mult_sigmaR,
#endif
     &                    mult_temp,
     &                    mult_salt,
     &                    mult_temp0,
     &                    mult_salt0,
     &                    mult_etan0,
     &                    mult_uvel0,
     &                    mult_vvel0,
     &                    mult_sst,
     &                    mult_tmi,
     &                    mult_sss,
     &                    mult_bp,
     &                    mult_ies,
     &                    mult_ctdt,
     &                    mult_ctds,
     &                    mult_ctdtclim,
     &                    mult_ctdsclim,
     &                    mult_xbt,
     &                    mult_argot,
     &                    mult_argos,
     &                    mult_usercost,
     &                    mult_drift,
     &                    mult_tdrift,
     &                    mult_sdrift,
     &                    mult_wdrift,
     &                    mult_scatx,
     &                    mult_scaty,
     &                    mult_atemp,
     &                    mult_aqh,
     &                    mult_precip,
     &                    mult_swflux,
     &                    mult_swdown,
     &                    mult_snowprecip,
     &                    mult_lwflux,
     &                    mult_lwdown,
     &                    mult_evap,
     &                    mult_apressure,
     &                    mult_runoff,
     &                    mult_uwind,
     &                    mult_vwind,
     &                    mult_curmtr,
     &                    mult_kapgm,
     &                    mult_kapredi,
     &                    mult_diffkr,
     &                    mult_ini_fin,
     &                    mult_edtau,
     &                    mult_bottomdrag,
     &                    mult_smooth_ic,
     &                    mult_smooth_bc,
     &                    mult_transp
      _RL  mult_hflux
      _RL  mult_sflux
      _RL  mult_hfluxmm
      _RL  mult_sfluxmm
      _RL  mult_tauu
      _RL  mult_tauv
      _RL  mult_hmean
      _RL  mult_h
      _RL  mult_tp
      _RL  mult_ers
      _RL  mult_gfo
      _RL  mult_sshv4cost(NSSHV4COST)
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      _RL  mult_sigmaR
#endif
      _RL  mult_temp
      _RL  mult_salt
      _RL  mult_temp0
      _RL  mult_salt0
      _RL  mult_etan0
      _RL  mult_uvel0
      _RL  mult_vvel0
      _RL  mult_sst
      _RL  mult_tmi
      _RL  mult_sss
      _RL  mult_bp
      _RL  mult_ies
      _RL  mult_ctdt
      _RL  mult_ctds
      _RL  mult_ctdtclim
      _RL  mult_ctdsclim
      _RL  mult_xbt
      _RL  mult_argot
      _RL  mult_argos
      _RL  mult_usercost(NUSERCOST)
      _RL  mult_drift
      _RL  mult_tdrift
      _RL  mult_sdrift
      _RL  mult_wdrift
      _RL  mult_scatx
      _RL  mult_scaty
      _RL  mult_atemp
      _RL  mult_aqh
      _RL  mult_precip
      _RL  mult_swflux
      _RL  mult_swdown
      _RL  mult_snowprecip
      _RL  mult_lwflux
      _RL  mult_lwdown
      _RL  mult_evap
      _RL  mult_apressure
      _RL  mult_runoff
      _RL  mult_uwind
      _RL  mult_vwind
      _RL  mult_curmtr
      _RL  mult_kapgm
      _RL  mult_kapredi
      _RL  mult_diffkr
      _RL  mult_ini_fin
      _RL  mult_edtau
      _RL  mult_bottomdrag
      _RL  mult_smooth_ic
      _RL  mult_smooth_bc
      _RL  mult_transp

c     Data files for the weights used in the cost function:
c     =====================================================
c
c     hflux_errfile         - heat flux error.
c     sflux_errfile         - salt flux error.
c     tauu_errfile          - zonal wind stress error.
c     tauum_errfile         - zonal wind stress error.
c     tauv_errfile          - meridional wind stress error.
c     tauvm_errfile         - meridional wind stress error.
c     tscatx_errfile        - zonal wind stress error.
c     tscaty_errfile        - meridional wind stress error.
c     data_errfile          - weights for theta, salt, and SST
c     geoid_errfile         - geoid error.
c     geoid_covariancefile  - geoid error covariance.
c     ssh_errfile           - sea surface height error.
c     ctdt_errfile          - CTD temperature error.
c     ctds_errfile          - CTD salinity error.
c     drift_errfile         - drifter error.
c     salterrfile           - representation error due unresolved eddies
c     temperrfile           - representation error due unresolved eddies
c     sigmaRerrfile         - representation error due unresolved eddies
c     velerrfile            - representation error

      common /ecco_cost_c/
     &                hflux_errfile,
     &                hfluxm_errfile,
     &                sflux_errfile,
     &                sfluxm_errfile,
     &                tauu_errfile,
     &                tauum_errfile,
     &                tauv_errfile,
     &                tauvm_errfile,
     &                scatx_errfile,
     &                scaty_errfile,
     &                data_errfile,
     &                geoid_errfile,
     &                geoid_covariancefile,
     &                ssh_errfile,
     &                tp_errfile,
     &                ers_errfile,
     &                gfo_errfile,
     &                sshv4cost_scalefile,
     &                sshv4cost_errfile,
     &                ctdt_errfile,
     &                ctds_errfile,
     &                drift_errfile,
     &                udrifterrfile,
     &                vdrifterrfile,
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
     &                sigmaRerrfile,
#endif
     &                salterrfile,
     &                temperrfile,
     &                velerrfile,
     &                salt0errfile,
     &                temp0errfile,
     &                etan0errfile, 
     &                uvel0errfile,
     &                vvel0errfile,
     &                vel0errfile,
     &                ssterrfile,
     &                ssserrfile,
     &                bperrfile,
     &                ieserrfile,
     &                atemp_errfile,
     &                aqh_errfile,
     &                precip_errfile,
     &                swflux_errfile,
     &                swdown_errfile,
     &                snowprecip_errfile,
     &                lwflux_errfile,
     &                lwdown_errfile,
     &                evap_errfile,
     &                apressure_errfile,
     &                runoff_errfile,
     &                edtau_errfile,
     &                kapgm_errfile,
     &                kapredi_errfile,
     &                diffkr_errfile,
     &                bottomdrag_errfile,
     &                usercost_errfile,
     &                uwind_errfile,
     &                vwind_errfile
      character*(MAX_LEN_FNAM) hflux_errfile
      character*(MAX_LEN_FNAM) sflux_errfile
      character*(MAX_LEN_FNAM) tauu_errfile
      character*(MAX_LEN_FNAM) tauv_errfile
      character*(MAX_LEN_FNAM) hfluxm_errfile
      character*(MAX_LEN_FNAM) sfluxm_errfile
      character*(MAX_LEN_FNAM) tauum_errfile
      character*(MAX_LEN_FNAM) tauvm_errfile
      character*(MAX_LEN_FNAM) scatx_errfile
      character*(MAX_LEN_FNAM) scaty_errfile
      character*(MAX_LEN_FNAM) data_errfile
      character*(MAX_LEN_FNAM) geoid_errfile
      character*(MAX_LEN_FNAM) geoid_covariancefile
      character*(MAX_LEN_FNAM) ssh_errfile
      character*(MAX_LEN_FNAM) tp_errfile
      character*(MAX_LEN_FNAM) ers_errfile
      character*(MAX_LEN_FNAM) gfo_errfile
      character*(MAX_LEN_FNAM) sshv4cost_scalefile(NSSHV4COST)
      character*(MAX_LEN_FNAM) sshv4cost_errfile(NSSHV4COST)
      character*(MAX_LEN_FNAM) ctdt_errfile
      character*(MAX_LEN_FNAM) ctds_errfile
      character*(MAX_LEN_FNAM) drift_errfile
      character*(MAX_LEN_FNAM) udrifterrfile
      character*(MAX_LEN_FNAM) vdrifterrfile
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      character*(MAX_LEN_FNAM) sigmaRerrfile
#endif
      character*(MAX_LEN_FNAM) salterrfile
      character*(MAX_LEN_FNAM) temperrfile
      character*(MAX_LEN_FNAM) velerrfile
      character*(MAX_LEN_FNAM) salt0errfile
      character*(MAX_LEN_FNAM) temp0errfile
      character*(MAX_LEN_FNAM) etan0errfile
      character*(MAX_LEN_FNAM) uvel0errfile
      character*(MAX_LEN_FNAM) vvel0errfile
      character*(MAX_LEN_FNAM) vel0errfile
      character*(MAX_LEN_FNAM) ssterrfile
      character*(MAX_LEN_FNAM) ssserrfile
      character*(MAX_LEN_FNAM) bperrfile
      character*(MAX_LEN_FNAM) ieserrfile
      character*(MAX_LEN_FNAM) atemp_errfile
      character*(MAX_LEN_FNAM) aqh_errfile
      character*(MAX_LEN_FNAM) precip_errfile
      character*(MAX_LEN_FNAM) swflux_errfile
      character*(MAX_LEN_FNAM) swdown_errfile
      character*(MAX_LEN_FNAM) snowprecip_errfile
      character*(MAX_LEN_FNAM) lwflux_errfile
      character*(MAX_LEN_FNAM) lwdown_errfile
      character*(MAX_LEN_FNAM) evap_errfile
      character*(MAX_LEN_FNAM) apressure_errfile
      character*(MAX_LEN_FNAM) runoff_errfile
      character*(MAX_LEN_FNAM) edtau_errfile
      character*(MAX_LEN_FNAM) kapgm_errfile
      character*(MAX_LEN_FNAM) kapredi_errfile
      character*(MAX_LEN_FNAM) diffkr_errfile
      character*(MAX_LEN_FNAM) bottomdrag_errfile
      character*(MAX_LEN_FNAM) usercost_errfile(NUSERCOST)
      character*(MAX_LEN_FNAM) uwind_errfile
      character*(MAX_LEN_FNAM) vwind_errfile


c     Arrays where the weights are stored:
c     ====================================
c
c     cosphi     - cosine of latitude.
c     whflux     - weight for heat flux.
c     wsflux     - weight for salt flux.
c     wtauu      - weight for zonal wind stress.
c     wtauu      - weight for meridional wind stress.
c     wscatx     - weight for zonal scat stress.
c     wscaty     - weight for meridional scat stress.
c     wtheta     - weight for temperature.
c     wtheta2    - representation error due to unresolved eddies
c     wsst       - weight for sea surface temperature.
c     wsss       - weight for sea surface salinity.
c     wsalt      - weight for salinity.
c     wsalt2     - representation error due to unresolved eddies
c     wsigmaR    - weight for sigmaR
c     wsigmaR2   - representation error due to unresolved eddies
c     wtp        - weight for TOPEX/POSEIDON data.
c     wers       - weight for ERS data.
c     wp         - weight for geoid.
c     wctdt      - weight for CTD temperature.
c     wctds      - weight for CTD salinity.
c     wudrift    - weight for mean zonal velocity from drifters.
c     wvdrift    - weight for mean meridional velocity from drifters.
c     wetan      - weight for etan0

      common /ecco_cost_weights_r/
     &                      whflux,wsflux,wtauu,wtauv,
     &                      watemp,waqh,wprecip,wsnowprecip,
     &                      wswflux,wswdown,wlwflux,wlwdown,
     &                      wevap,wapressure,wrunoff,
     &                      wbottomdrag,
     &                      wuwind,wvwind,
     &                      wscatx,wscaty,
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
     &                      wsigmaR,wsigmaR2,wsigmaRLev,
#endif
     &                      wtheta,wtheta2,wthetaLev,
     &                      wsalt,wsalt2,wsaltLev,
     &                      wdiffkr,wdiffkr2,wdiffkrFld,
     &                      wkapgm,wkapgm2,wkapgmFld,
     &                      wkapredi,wkapredi2,wkaprediFld,
     &                      wedtaux,wedtaux2,wedtauxFld,
     &                      wedtauy,wedtauy2,wedtauyFld,
     &                      wsst,wsss,wbp, wies,
     &                      wtp,wers,wgfo,
     &                      wp,wsshv4,
     &                      wctdt,wctds,
     &                      wudrift,wvdrift,
     &                      whfluxmm,wsfluxmm,
     &                      wcurrent,wcurrent2,
     &                      wcurrentLev,wbaro,wetan,
     &                      wuvel,wvvel

      _RL whflux  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL whfluxm (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL whfluxmm(1-olx:snx+olx,1-oly:sny+oly)
      _RL wsflux  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wsfluxm (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wsfluxmm(1-olx:snx+olx,1-oly:sny+oly)
      _RL wtauu   (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wtauv   (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wtauum  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wtauvm  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wscatx  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wscaty  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL watemp  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL waqh    (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wprecip (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wswflux (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wswdown (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wsnowprecip (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wlwflux (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wlwdown (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wevap   (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wapressure(1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wrunoff (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wbottomdrag (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL wuwind  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wvwind  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wtheta  (                            nr,nsx,nsy)
      _RL wsalt   (                            nr,nsx,nsy)
      _RL wtheta2 (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wsalt2  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wthetaLev (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wsaltLev  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      _RL wsigmaR   (                            nr,nsx,nsy)
      _RL wsigmaR2  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wsigmaRLev(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif
      _RL wuvel   (                            nr,nsx,nsy)
      _RL wvvel   (                            nr,nsx,nsy)
      _RL wsst    (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wsss    (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wbp     (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wies     (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wtp     (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wsshv4  (1-olx:snx+olx,1-oly:sny+oly,NSSHV4COST,nsx,nsy)
      _RL wers    (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wgfo    (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wp      (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wctdt   (                            nr,nsx,nsy)
      _RL wctds   (                            nr,nsx,nsy)
      _RL wudrift (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wvdrift (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wcurrent(                              nr,nsx,nsy)
      _RL wcurrent2   (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wcurrentLev (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wbaro
      _RL wdiffkr (                            nr,nsx,nsy)
      _RL wdiffkr2 (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wdiffkrFld (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wkapgm (                            nr,nsx,nsy)
      _RL wkapgm2 (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wkapgmFld (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wkapredi (                            nr,nsx,nsy)
      _RL wkapredi2 (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wkaprediFld (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wedtaux (                            nr,nsx,nsy)
      _RL wedtaux2 (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wedtauxFld (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wedtauy (                            nr,nsx,nsy)
      _RL wedtauy2 (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wedtauyFld (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wetan   (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)

#if (defined (ALLOW_UVEL0_COST_CONTRIBUTION) || defined (ALLOW_UVEL0_CONTROL))
#if (defined (ALLOW_VVEL0_COST_CONTRIBUTION) || defined (ALLOW_VVEL0_CONTROL))
c     wuvel3d      - weight for uvel0
c     wvvel3d      - weight for vvel0
c
      common /ecco_cost_weights_vel_r/
     &                      wuvel3d, wvvel3d  
c
      _RL wuvel3d(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wvvel3d(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif
#endif

      common /ecco_cost_weights_0_r/
     &        whflux0, wsflux0, wtau0,
     &        watemp0, waqh0, wprecip0, wsnowprecip0, wwind0,
     &        wswflux0, wswdown0, wlwflux0, wlwdown0,
     &        wevap0, wapressure0, wrunoff0, wkapredi0,
     &        wbottomdrag0,wdiffkr0, wkapgm0, wedtau0
      _RL whflux0
      _RL wsflux0
      _RL wtau0
      _RL watemp0
      _RL waqh0
      _RL wprecip0
      _RL wswflux0
      _RL wswdown0
      _RL wsnowprecip0
      _RL wlwflux0
      _RL wlwdown0
      _RL wevap0
      _RL wapressure0
      _RL wrunoff0
      _RL wbottomdrag0
      _RL wwind0
      _RL wdiffkr0
      _RL wkapgm0
      _RL wkapredi0
      _RL wedtau0

      common /ecco_cost_weights_mean_r/
     &        wmean_hflux, wmean_sflux, wmean_tau,
     &        wmean_atemp, wmean_aqh,
     &        wmean_precip, wmean_snowprecip, wmean_wind,
     &        wmean_swflux, wmean_swdown, wmean_lwflux, wmean_lwdown,
     &        wmean_evap, wmean_apressure, wmean_runoff
      _RL wmean_hflux
      _RL wmean_sflux
      _RL wmean_tau
      _RL wmean_atemp
      _RL wmean_aqh
      _RL wmean_precip
      _RL wmean_swflux
      _RL wmean_swdown
      _RL wmean_snowprecip
      _RL wmean_lwflux
      _RL wmean_lwdown
      _RL wmean_evap
      _RL wmean_apressure
      _RL wmean_runoff
      _RL wmean_wind

      common /ecco_cost_weights_2_r/
     &                      whflux2,wsflux2,wtauu2,wtauv2
      _RL whflux2 (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wsflux2 (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wtauu2  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wtauv2  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)

c     Arrays that contain observations for the model-data comparison:
c     ===============================================================
c
c     sigmaRdat  - reference sigmaR data.
c     tdat       - reference temperature data.
c     scatxdat   - reference zonal wind stress.
c     scatydat   - reference meridional wind stress.
c     sstdat     - reference sea surface temperature data.
c     tmidat     - reference TMI sea surface temperature data.
c     sssdat     - reference sea surface temperature data.
c     bpdat      - bottom pressure from time-varying GRACE.
c     iesdat     - roundtrip travel time from IES
c     tauxmask   - mask for reference wind stress data.
c     tauymask   - mask for reference wind stress data.
c     scatxmask  - mask for scat wind stress data.
c     scatymask  - mask for scat wind stress data.
c     sstmask    - mask for reference sea surface temperature data.
c     tmimask    - mask for reference sea surface temperature data.
c     sssmask    - mask for reference sea surface temperature data.
c     sdat       - reference salinity data.
c     mdt        - reference mean sea surface height data.
c     mdtmask    - mask for reference mean sea surface height data.
c     tpobs      - TOPEX/POSEIDON data.
c     tpmask     - mask for TOPEX/POSEIDON data.
c     ersobs     - ERS data.
c     ersmask    - mask for ERS data.
c     ctdtobs    - CTD temperature data
c     ctdsobs    - CTD salinity data
c     xbtobs     - XBT data
c     argot      - ARGO  temperature data
c     argos      - ARGO  salt data
c     udriftdat  - drifters zonal velocities
c     vdriftdat  - drifters meridional velocities

      common /ecco_cost_data_r/
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
     &                     sigmaRdat,
#endif
     &                     tdat,
     &                     scatxdat,
     &                     scatydat,
     &                     bpdat,
     &                     iesdat,
     &                     sstmask,
     &                     tmimask,
     &                     sssmask,
     &                     bpmask,
     &                     iesmask,
     &                     tauxmask,
     &                     tauymask,
     &                     scatxmask,
     &                     scatymask,
     &                     sdat,
     &                     mdt,
     &                     mdtmask,
     &                     tpobs,
     &                     tpmask,
     &                     ersobs,
     &                     ersmask,
     &                     gfoobs,
     &                     gfomask,
     &                     ctdtobs,
     &                     ctdsobs,
     &                     xbtobs,
     &                     argotobs,
     &                     argosobs,
     &                     udriftdat,
     &                     vdriftdat,
     &                     curmtruobs,
     &                     curmtrvobs

#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      _RL sigmaRdat (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif
      _RL tdat      (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL scatxdat  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL scatydat  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL bpdat     (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL iesdat     (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL tauxmask  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL tauymask  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL scatxmask (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL scatymask (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL sstmask   (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL tmimask   (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL sssmask   (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL bpmask    (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL iesmask    (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL sdat      (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL mdt       (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL mdtmask   (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL tpobs     (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL tpmask    (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL ersobs    (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL ersmask   (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL gfoobs    (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL gfomask   (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL ctdtobs   (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL ctdsobs   (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL xbtobs    (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL argotobs  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL argosobs  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL udriftdat (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL vdriftdat (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL curmtruobs(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL curmtrvobs(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)


c     Files that contain obervations:
c     ===============================
c
C     sigmaRdatfile - reference data file for sigmaR
c     tdatfile      - reference data file for temperature.
c     sdatfile      - reference data file for salinity.
c     scatxdatfile  - reference data file for zonal wind stress.
c     scatydatfile  - reference data file for meridional wind stress.
c     sstdatfile    - reference data file for sea surface temperature.
c     tmidatfile    - reference data file for TMI sea surface temperature.
c     mdtdatfile    - reference data file for mean sea surface height.
c     topexfile     - reference data file for sea surface height data
c                     (TOPEX/POSEIDON).
c     ersfile       - reference data file for sea surface height data
c                     (ERS).
c ctdtfile, ctdsfile- reference data file for temperature and salinity
c                     from CTD
c ctdtclimfile, ctdsclimfile- reference data file for temperature
c                      and salinity from CTD with out timetag
c     xbtfile       - reference data file for xbt
c     ARGOtfile     - reference data file for ARGO
c     ARGOsfile     - reference data file for ARGO
c     driftfile     - reference data file for drifter mean velocities

      common /ecco_cost_data_c/
#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
     &                     sigmaRdatfile,
#endif
     &                     tdatfile,
     &                     sdatfile,
     &                     scatxdatfile,
     &                     scatydatfile,
     &                     sstdatfile,
     &                     tmidatfile,
     &                     sssdatfile,
     &                     bpdatfile,
     &                     iesdatfile,
     &                     mdtdatfile,
     &                     topexfile,
     &                     ersfile,
     &                     gfofile,
     &                     ctdtfile,
     &                     ctdsfile,
     &                     ctdtclimfile,
     &                     ctdsclimfile,
     &                     xbtfile,
     &                     argotfile,
     &                     argosfile,
     &                     udriftfile,
     &                     vdriftfile,
     &                     usercost_datafile,
     &                     curmtrufile,
     &                     curmtrvfile

#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
      character*(MAX_LEN_FNAM) sigmaRdatfile
#endif
      character*(MAX_LEN_FNAM) tdatfile
      character*(MAX_LEN_FNAM) sdatfile
      character*(MAX_LEN_FNAM) scatxdatfile
      character*(MAX_LEN_FNAM) scatydatfile
      character*(MAX_LEN_FNAM) sstdatfile
      character*(MAX_LEN_FNAM) tmidatfile
      character*(MAX_LEN_FNAM) sssdatfile
      character*(MAX_LEN_FNAM) bpdatfile
      character*(MAX_LEN_FNAM) iesdatfile
      character*(MAX_LEN_FNAM) mdtdatfile
      character*(MAX_LEN_FNAM) topexfile
      character*(MAX_LEN_FNAM) ersfile
      character*(MAX_LEN_FNAM) gfofile
      character*(MAX_LEN_FNAM) ctdtfile
      character*(MAX_LEN_FNAM) ctdsfile
      character*(MAX_LEN_FNAM) ctdtclimfile
      character*(MAX_LEN_FNAM) ctdsclimfile
      character*(MAX_LEN_FNAM) xbtfile
      character*(MAX_LEN_FNAM) argotfile
      character*(MAX_LEN_FNAM) argosfile
      character*(MAX_LEN_FNAM) argofile
      character*(MAX_LEN_FNAM) usercost_datafile(NUSERCOST)
      character*(MAX_LEN_FNAM) udriftfile
      character*(MAX_LEN_FNAM) vdriftfile
      character*(MAX_LEN_FNAM) curmtrufile
      character*(MAX_LEN_FNAM) curmtrvfile

c     Calendar information for the observations:
c     ==========================================
c
c     sststartdate   - start date of the sea surface temperature data.
c     tmistartdate   - start date of the sea surface temperature data.
c     topexstartdate - start date of the sea surface height data.
c     ersstartdate   - start date of the sea surface height data.
c     sshperiod      - sampling interval for the sea surface height data.

      common /ecco_cost_data_times_i/
     &                           scatxstartdate,
     &                           scatystartdate,
     &                           sststartdate,
     &                           argotstartdate,
     &                           argosstartdate,
     &                           tmistartdate,
     &                           sssstartdate,
     &                           bpstartdate,
     &                           iesstartdate,
     &                           topexstartdate,
     &                           ersstartdate,
     &                           gfostartdate,
     &                           mdtstartdate,
     &                           mdtenddate
      integer scatxstartdate(4)
      integer scatystartdate(4)
      integer sststartdate(4)
      integer argotstartdate(4)
      integer argosstartdate(4)
      integer tmistartdate(4)
      integer sssstartdate(4)
      integer bpstartdate(4)
      integer iesstartdate(4)
      integer topexstartdate(4)
      integer ersstartdate(4)
      integer gfostartdate(4)
      integer mdtstartdate(4)
      integer mdtenddate(4)

      common /ecco_cost_data_aux_i/
     &                           tmistartdate1,
     &                           tmistartdate2,
     &                           sststartdate1,
     &                           sststartdate2,
     &                           sssstartdate1,
     &                           sssstartdate2,
     &                           bpstartdate1,
     &                           bpstartdate2,
     &                           iesstartdate1,
     &                           iesstartdate2,
     &                           argotstartdate1,
     &                           argotstartdate2,
     &                           argosstartdate1,
     &                           argosstartdate2,
     &                           topexstartdate1,
     &                           topexstartdate2,
     &                           ersstartdate1,
     &                           ersstartdate2,
     &                           gfostartdate1,
     &                           gfostartdate2,
     &                           scatstartdate1,
     &                           scatstartdate2,
     &                           mdtstartdate1,
     &                           mdtstartdate2,
     &                           mdtenddate1,
     &                           mdtenddate2

      integer tmistartdate1
      integer tmistartdate2
      integer sststartdate1
      integer sststartdate2
      integer sssstartdate1
      integer sssstartdate2
      integer bpstartdate1
      integer bpstartdate2
      integer iesstartdate1
      integer iesstartdate2
      integer argotstartdate1
      integer argotstartdate2
      integer argosstartdate1
      integer argosstartdate2
      integer topexstartdate1
      integer topexstartdate2
      integer ersstartdate1
      integer ersstartdate2
      integer gfostartdate1
      integer gfostartdate2
      integer scatstartdate1
      integer scatstartdate2
      integer mdtstartdate1
      integer mdtstartdate2
      integer mdtenddate1
      integer mdtenddate2

      common /ecco_cost_data_times_r/
     &                           topexperiod,
     &                           ersperiod,
     &                           gfoperiod,
     &                           scatperiod
      _RL topexperiod
      _RL ersperiod
      _RL gfoperiod
      _RL scatperiod

      common /ecco_cost_data_detrend/
     &                           topexintercept,
     &                           ersintercept,
     &                           gfointercept,
     &                           topexslope,
     &                           ersslope,
     &                           gfoslope
      _RL topexintercept
      _RL ersintercept
      _RL gfointercept
      _RL topexslope
      _RL ersslope
      _RL gfoslope

cgf factor to convert sshv4cost_errfile in m
      common /ecco_cost_errfactor/
     &         sshv4cost_errfactor
      _RL  sshv4cost_errfactor(NSSHV4COST)

#ifdef ALLOW_SSH_COST_CONTRIBUTION
      common /ecco_ssh_daymask_r/
     &       tpTimeMask, ersTimeMask, gfoTimeMask
      _RL tpTimeMask(maxNumDays)
      _RL ersTimeMask(maxNumDays)
      _RL gfoTimeMask(maxNumDays)
#endif

      common /ecco_ssh_daymask_c/
     &       tpTimeMaskFile, ersTimeMaskFile, gfoTimeMaskFile
      character*(MAX_LEN_FNAM) tpTimeMaskFile
      character*(MAX_LEN_FNAM) ersTimeMaskFile
      character*(MAX_LEN_FNAM) gfoTimeMaskFile

#endif /* ECCO_CTRL_DEPRECATED */

c     ==================================================================
c     END OF HEADER COST
c     ==================================================================


