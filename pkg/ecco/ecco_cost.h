
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

c     Averaging counters:
c     ===================
c
c     sum1day - counter for the daily averaging
c     sum1mon - counter for the monthly averaging
c     dayrec  - number of averaged surface pressure records.
c     monrec  - number of averaged theta and salinity records.

      common /average_i/ 
     &                   sum1day,sum1mon,sum1year,
     &                   dayrec,monrec,yearrec
      integer sum1day
      integer sum1mon
      integer sum1year
      integer dayrec
      integer monrec
      integer yearrec


c     Averaged Fields:
c     ================
c
c     tbar  - contains the averaged temperature field after the call
c             to subroutine POST_MONTHLY. Before, it accumulates the
c             intantaneous temperatures.
c     sbar  - contains the averaged salinity field after the call
c             to subroutine POST_MONTHLY. Before, it accumulates the
c             intantaneous salinities.
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
c     hfluxbar  - contains the averaged zonal velocity component for the
c             whole integration period. Before, it accumulates the
c             intantaneous field.
c     sfluxbar  - contains the averaged zonal velocity component for the
c             whole integration period. Before, it accumulates the
c             intantaneous field.

      common /averages_r/ 
     &                    tbar,
     &                    sbar,
     &                    psbar,
     &                    ubar,
     &                    vbar,
     &                    wbar,
     &                    tauxbar,
     &                    tauybar,
     &                    hfluxbar,
     &                    sfluxbar,
     &                    Slmean, 
     &                    Tlmean,
     &                    wlmean,
     &                    Sfmean,
     &                    Tfmean,                   
     &			  sbar_gen,
     &			  tbar_gen,
     &                    wfmean

#if (defined (ALLOW_THETA_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDT_COST_CONTRIBUTION) || \
     defined (ALLOW_XBT_COST_CONTRIBUTION) || \
     defined (ALLOW_DRIFT_COST_CONTRIBUTION) || \
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

#if (defined (ALLOW_SALT_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDS_COST_CONTRIBUTION) || \
     defined (ALLOW_DRIFT_COST_CONTRIBUTION) || \
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

#if (defined (ALLOW_DRIFTER_COST_CONTRIBUTION) || \
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

#ifdef ALLOW_SCAT_COST_CONTRIBUTION
      _RL tauxbar  (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)
      _RL tauybar  (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)
#else
      _RL tauxbar 
      _RL tauybar 
#endif

#ifdef ALLOW_MEAN_HFLUX_COST_CONTRIBUTION
      _RL hfluxbar  (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)
#else 
      _RL hfluxbar
#endif

#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
      _RL sfluxbar  (1-olx:snx+olx,1-oly:sny+oly,  nsx,nsy)    
#else 
      _RL sfluxbar
#endif


      common /averages_c/ 
     &                    tbarfile,
     &                    sbarfile,
     &                    psbarfile,
     &                    ubarfile,
     &                    vbarfile,
     &                    wbarfile,
     &                    tauxbarfile,
     &                    tauybarfile,
     &                    hfluxbarfile,
     &                    sfluxbarfile
      character*(MAX_LEN_FNAM) tbarfile
      character*(MAX_LEN_FNAM) sbarfile
      character*(MAX_LEN_FNAM) psbarfile
      character*(MAX_LEN_FNAM) ubarfile
      character*(MAX_LEN_FNAM) vbarfile
      character*(MAX_LEN_FNAM) wbarfile
      character*(MAX_LEN_FNAM) tauxbarfile
      character*(MAX_LEN_FNAM) tauybarfile
      character*(MAX_LEN_FNAM) hfluxbarfile
      character*(MAX_LEN_FNAM) sfluxbarfile

c     file precision and field type

      common /prec_type_cost/ 
     &                        cost_iprec,
     &                        cost_yftype

      integer cost_iprec
      character*(2) cost_yftype

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
c     objf_scatxm - time-mean zonal SCAT  contribution
c     objf_scatym - time-mean meridional SCAT  contribution
c     objf_scatx  - zonal SCAT  contribution
c     objf_scaty  - meridional SCAT  contribution
c     objf_kapgm  - kappa GM contribution
c     objf_diffkr - diffusion contribution
c     objf_theta_ini_fin - final vs. initial theta misfit
c     objf_salt_ini_fin  - final vs. initial salt misfit
c
c     mult_"var" - multipliers for the individual cost
c                  function contributions.

      common /ecco_cost_objf/
     &                objf_hflux,
     &                objf_hfluxm,
     &                objf_hfluxmm,
     &                objf_hfluxsmoo,
     &                objf_sflux,
     &                objf_sfluxm,
     &                objf_sfluxmm,
     &                objf_sfluxsmoo,
     &                objf_tauu,
     &                objf_tauum,
     &                objf_tauusmoo,
     &                objf_tauv,
     &                objf_tauvm,
     &                objf_tauvsmoo,
     &                objf_hmean,
     &                objf_h,
     &                objf_tp,
     &                objf_ers,
     &                objf_gfo,
     &                objf_temp,
     &                objf_salt,
     &                objf_temp0,
     &                objf_salt0,
     &                objf_temp0smoo,
     &                objf_salt0smoo,
     &                objf_sst,
     &                objf_tmi,
     &                objf_sss,
     &                objf_ctdt,
     &                objf_ctds,
     &                objf_ctdtclim,
     &                objf_ctdsclim,
     &                objf_xbt,
     &                objf_argot,
     &                objf_argos,
     &                objf_drift,
     &                objf_tdrift,
     &                objf_sdrift,
     &                objf_wdrift,
     &                objf_scatx,
     &                objf_scaty,
     &                objf_scatxm,
     &                objf_scatym,
     &                objf_atemp,
     &                objf_aqh,
     &                objf_precip,
     &                objf_swflux,
     &                objf_swdown,
     &                objf_snowprecip,
     &                objf_lwflux,
     &                objf_lwdown,
     &                objf_evap,
     &                objf_apressure,
     &                objf_runoff,
     &                objf_uwind,
     &                objf_vwind,
     &                objf_atempm,
     &                objf_aqhm,
     &                objf_precipm,
     &                objf_swfluxm,
     &                objf_swdownm,
     &                objf_snowprecipm,
     &                objf_lwfluxm,
     &                objf_lwdownm,
     &                objf_evapm,
     &                objf_apressurem,
     &                objf_runoffm,
     &                objf_uwindm,
     &                objf_vwindm,
     &                objf_atempsmoo,
     &                objf_aqhsmoo,
     &                objf_precipsmoo,
     &                objf_swfluxsmoo,
     &                objf_swdownsmoo,
     &                objf_snowprecipsmoo,
     &                objf_lwfluxsmoo,
     &                objf_lwdownsmoo,
     &                objf_evapsmoo,
     &                objf_apressuresmoo,
     &                objf_runoffsmoo,
     &                objf_uwindsmoo,
     &                objf_vwindsmoo,
     &                objf_obcsn,
     &                objf_obcss,
     &                objf_obcsw,
     &                objf_obcse,
     &                objf_obcsvol,
     &                objf_curmtr,
     &                objf_ageos,
     &                objf_kapgm,
     &                objf_diffkr,
     &                objf_theta_ini_fin,
     &                objf_salt_ini_fin,
     &                objf_eddytau
      _RL  objf_hflux  (nsx,nsy)
      _RL  objf_hfluxm (nsx,nsy)
      _RL  objf_hfluxmm(nsx,nsy)
      _RL  objf_hfluxsmoo  (nsx,nsy)
      _RL  objf_sflux  (nsx,nsy)
      _RL  objf_sfluxm (nsx,nsy)
      _RL  objf_sfluxmm(nsx,nsy)
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
      _RL  objf_temp (nsx,nsy)
      _RL  objf_salt (nsx,nsy)
      _RL  objf_temp0(nsx,nsy)
      _RL  objf_salt0(nsx,nsy)
      _RL  objf_temp0smoo(nsx,nsy)
      _RL  objf_salt0smoo(nsx,nsy)
      _RL  objf_sst  (nsx,nsy)
      _RL  objf_tmi  (nsx,nsy)
      _RL  objf_sss  (nsx,nsy) 
      _RL  objf_ctdt (nsx,nsy)
      _RL  objf_ctds (nsx,nsy)
      _RL  objf_ctdtclim (nsx,nsy)
      _RL  objf_ctdsclim (nsx,nsy)
      _RL  objf_xbt  (nsx,nsy)
      _RL  objf_argot(nsx,nsy)
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
      _RL  objf_obcsn(nsx,nsy)
      _RL  objf_obcss(nsx,nsy)
      _RL  objf_obcsw(nsx,nsy)
      _RL  objf_obcse(nsx,nsy)
      _RL  objf_obcsvol
      _RL  objf_curmtr(nsx,nsy)
      _RL  objf_ageos(nsx,nsy)
      _RL  objf_kapgm(nsx,nsy)
      _RL  objf_diffkr(nsx,nsy)
      _RL  objf_theta_ini_fin(nsx,nsy)
      _RL  objf_salt_ini_fin(nsx,nsy)
      _RL  objf_eddytau(nsx,nsy)

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
     &                num_temp,
     &                num_salt,
     &                num_temp0,
     &                num_salt0,
     &                num_sst,
     &                num_tmi,
     &                num_sss,
     &                num_ctdt,
     &                num_ctds,
     &                num_ctdtclim,
     &                num_ctdsclim,
     &                num_xbt,
     &                num_argot,
     &                num_argos,
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
     &                num_obcsn,
     &                num_obcss,
     &                num_obcsw,
     &                num_obcse,
     &                num_obcsvol,
     &                num_curmtr,
     &                num_ageos,
     &                num_kapgm,
     &                num_diffkr,
     &                num_theta_ini_fin,
     &                num_salt_ini_fin,
     &                num_eddytau

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
      _RL  num_temp (nsx,nsy)
      _RL  num_salt (nsx,nsy)
      _RL  num_temp0(nsx,nsy)
      _RL  num_salt0(nsx,nsy)
      _RL  num_sst  (nsx,nsy)
      _RL  num_tmi  (nsx,nsy)
      _RL  num_sss  (nsx,nsy) 
      _RL  num_ctdt (nsx,nsy)
      _RL  num_ctds (nsx,nsy)
      _RL  num_ctdtclim (nsx,nsy)
      _RL  num_ctdsclim (nsx,nsy)
      _RL  num_xbt  (nsx,nsy)
      _RL  num_argot(nsx,nsy)
      _RL  num_argos(nsx,nsy)
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
      _RL  num_obcsn(nsx,nsy)
      _RL  num_obcss(nsx,nsy)
      _RL  num_obcsw(nsx,nsy)
      _RL  num_obcse(nsx,nsy)
      _RL  num_obcsvol
      _RL  num_curmtr(nsx,nsy)
      _RL  num_ageos(nsx,nsy)
      _RL  num_kapgm(nsx,nsy)
      _RL  num_diffkr(nsx,nsy)
      _RL  num_theta_ini_fin(nsx,nsy)
      _RL  num_salt_ini_fin(nsx,nsy)
      _RL  num_eddytau(nsx,nsy)

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
     &                    mult_temp,
     &                    mult_salt,
     &                    mult_temp0,
     &                    mult_salt0,
     &                    mult_sst,
     &                    mult_tmi,
     &                    mult_sss,
     &                    mult_ctdt,
     &                    mult_ctds,
     &                    mult_ctdtclim,
     &                    mult_ctdsclim,
     &                    mult_xbt,
     &                    mult_argot,
     &                    mult_argos,
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
     &                    mult_obcsn,
     &                    mult_obcss,
     &                    mult_obcsw,
     &                    mult_obcse,
     &                    mult_obcsvol,
     &                    mult_curmtr,
     &                    mult_ageos,
     &                    mult_kapgm,
     &                    mult_diffkr,
     &                    mult_ini_fin,
     &                    mult_eddytau,
     &                    mult_smooth_ic,
     &                    mult_smooth_bc
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
      _RL  mult_temp
      _RL  mult_salt
      _RL  mult_temp0
      _RL  mult_salt0
      _RL  mult_sst
      _RL  mult_tmi
      _RL  mult_sss
      _RL  mult_ctdt
      _RL  mult_ctds
      _RL  mult_ctdtclim
      _RL  mult_ctdsclim
      _RL  mult_xbt
      _RL  mult_argot
      _RL  mult_argos
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
      _RL  mult_obcsn
      _RL  mult_obcss
      _RL  mult_obcsw
      _RL  mult_obcse
      _RL  mult_obcsvol
      _RL  mult_curmtr
      _RL  mult_ageos
      _RL  mult_kapgm
      _RL  mult_diffkr
      _RL  mult_ini_fin
      _RL  mult_eddytau
      _RL  mult_smooth_ic
      _RL  mult_smooth_bc

c     Record counters relevant for the cost function evaluation.
c     ==========================================================
c
c     nyearsrec - number of yearly records that will be generated by
c                 the current model integration.
c     nmonsrec  - number of monthly records that will be generated by
c                 the current model integration.
c     ndaysrec  - number of  daily  records that will be generated by
c                 the current model integration.

      common /ecco_cost_i/
     &                nyearsrec,
     &                nmonsrec,
     &                ndaysrec,
     &                nnztbar,
     &                nnzsbar
      integer nyearsrec
      integer nmonsrec
      integer ndaysrec
      integer nnztbar
      integer nnzsbar


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
     &                ctdt_errfile,
     &                ctds_errfile, 
     &                drift_errfile,
     &                udrifterrfile, 
     &                vdrifterrfile, 
     &                salterrfile,
     &                temperrfile,
     &                velerrfile,
     &                salt0errfile,
     &                temp0errfile,
     &                vel0errfile,
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
      character*(MAX_LEN_FNAM) ctdt_errfile 
      character*(MAX_LEN_FNAM) ctds_errfile 
      character*(MAX_LEN_FNAM) drift_errfile
      character*(MAX_LEN_FNAM) udrifterrfile
      character*(MAX_LEN_FNAM) vdrifterrfile      
      character*(MAX_LEN_FNAM) salterrfile
      character*(MAX_LEN_FNAM) temperrfile
      character*(MAX_LEN_FNAM) velerrfile
      character*(MAX_LEN_FNAM) salt0errfile
      character*(MAX_LEN_FNAM) temp0errfile
      character*(MAX_LEN_FNAM) vel0errfile
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
c     wtp        - weight for TOPEX/POSEIDON data.
c     wers       - weight for ERS data.
c     wp         - weight for geoid.
c     wctdt      - weight for CTD temperature.
c     wctds      - weight for CTD salinity.
c     wudrift    - weight for mean zonal velocity from drifters.
c     wvdrift    - weight for mean meridional velocity from drifters.

      common /ecco_cost_weights_r/ 
     &                      frame,
     &                      cosphi,
     &                      whflux,wsflux,wtauu,wtauv,
     &                      watemp,waqh,wprecip,wsnowprecip,
     &                      wswflux,wswdown,wlwflux,wlwdown,
     &                      wevap,wapressure,wrunoff,
     &                      wuwind,wvwind,
     &                      wscatx,wscaty,
     &                      wtheta,wtheta2,wthetaLev,
     &                      wsalt,wsalt2,wsaltLev,
     &                      wdiffkr,wdiffkr2,wdiffkrFld,
     &                      wkapgm,wkapgm2,wkapgmFld,
     &                      wedtaux,wedtaux2,wedtauxFld,
     &                      wedtauy,wedtauy2,wedtauyFld,
     &                      wsst,wsss,
     &                      wtp,wers,wgfo,
     &                      wp,
     &                      wctdt,wctds,
     &                      wudrift,wvdrift,
     &                      whfluxmm,wsfluxmm,
     &                      wcurrent,wcurrent2,
     &                      wcurrentLev,wbaro

      _RL frame   (1-olx:snx+olx,1-oly:sny+oly           )
      _RL cosphi  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
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
      _RL wuwind  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wvwind  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wtheta  (                            nr,nsx,nsy)
      _RL wsalt   (                            nr,nsx,nsy)
      _RL wtheta2 (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wsalt2  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wthetaLev (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wsaltLev  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wsst    (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wsss    (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL wtp     (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
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
      _RL wedtaux (                            nr,nsx,nsy)
      _RL wedtaux2 (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wedtauxFld (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wedtauy (                            nr,nsx,nsy)
      _RL wedtauy2 (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL wedtauyFld (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

      common /ecco_cost_weights_0_r/
     &        whflux0, wsflux0, wtau0,
     &        watemp0, waqh0, wprecip0, wsnowprecip0, wwind0,
     &        wswflux0, wswdown0, wlwflux0, wlwdown0, 
     &        wevap0, wapressure0, wrunoff0,
     &        wdiffkr0, wkapgm0, wedtau0
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
      _RL wwind0
      _RL wdiffkr0
      _RL wkapgm0
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
						 
		      

#if (defined (ALLOW_OBCSN_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCSN_CONTROL))
      common /ecco_cost_weights_obcsn/
     &                      wobcsn, wobcsnLev
      _RL wobcsn     (                      nr,nobcs)
      _RL wobcsnLev  (1-olx:snx+olx,nr,nsx,nsy,nobcs)
#endif
#if (defined (ALLOW_OBCSS_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCSS_CONTROL))
      common /ecco_cost_weights_obcss/
     &                      wobcss, wobcssLev
      _RL wobcss     (                      nr,nobcs)
      _RL wobcssLev  (1-olx:snx+olx,nr,nsx,nsy,nobcs)
#endif
#if (defined (ALLOW_OBCSW_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCSW_CONTROL))
      common /ecco_cost_weights_obcsw/
     &                      wobcsw, wobcswLev
      _RL wobcsw     (                      nr,nobcs)
      _RL wobcswLev  (1-oly:sny+oly,nr,nsx,nsy,nobcs)
#endif
#if (defined (ALLOW_OBCSE_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCSE_CONTROL))
      common /ecco_cost_weights_obcse/
     &                      wobcse, wobcseLev
      _RL wobcse     (                      nr,nobcs)
      _RL wobcseLev  (1-oly:sny+oly,nr,nsx,nsy,nobcs)
#endif


c     Arrays that contain observations for the model-data comparison:
c     ===============================================================
c
c     tdat       - reference temperature data.
c     scatxdat   - reference zonal wind stress.
c     scatydat   - reference meridional wind stress.
c     sstdat     - reference sea surface temperature data.
c     tmidat     - reference TMI sea surface temperature data.
c     sssdat     - reference sea surface temperature data.
c     tauxmask   - mask for reference wind stress data.
c     tauymask   - mask for reference wind stress data. 
c     scatxmask  - mask for scat wind stress data.
c     scatymask  - mask for scat wind stress data. 
c     sstmask    - mask for reference sea surface temperature data.
c     tmimask    - mask for reference sea surface temperature data.
c     sssmask    - mask for reference sea surface temperature data.
c     sdat       - reference salinity data.
c     tpmean     - reference mean sea surface height data.
c     tpmeanmask - mask for reference mean sea surface height data.
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
     &                     tdat,
     &                     scatxdat,
     &                     scatydat,
     &                     sstdat,
     &                     tmidat,
     &                     sssdat,
     &                     sstmask,
     &                     tmimask,
     &                     sssmask,
     &                     tauxmask,
     &                     tauymask,
     &                     scatxmask,
     &                     scatymask,
     &                     sdat,
     &                     tpmean,
     &                     tpmeanmask,
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
     
      _RL tdat      (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL scatxdat  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL scatydat  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL sstdat    (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL tmidat    (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL sssdat    (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL tauxmask  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL tauymask  (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL scatxmask (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL scatymask (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL sstmask   (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL tmimask   (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL sssmask   (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL sdat      (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL tpmean    (1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
      _RL tpmeanmask(1-olx:snx+olx,1-oly:sny+oly,   nsx,nsy)
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
c     tdatfile      - reference data file for temperature.
c     sdatfile      - reference data file for salinity.
c     scatxdatfile  - reference data file for zonal wind stress.
c     scatydatfile  - reference data file for meridional wind stress.
c     sstdatfile    - reference data file for sea surface temperature.
c     tmidatfile    - reference data file for TMI sea surface temperature.
c     topexmeanfile - reference data file for mean sea surface height.
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
c     driftfile     - reference data file for drifter's mean velocities

      common /ecco_cost_data_c/
     &                     tdatfile,
     &                     sdatfile,
     &                     scatxdatfile,
     &                     scatydatfile,
     &                     sstdatfile,
     &                     tmidatfile,
     &                     sssdatfile,
     &                     topexmeanfile,
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
     &                     curmtrufile,
     &                     curmtrvfile

      character*(MAX_LEN_FNAM) tdatfile
      character*(MAX_LEN_FNAM) sdatfile
      character*(MAX_LEN_FNAM) scatxdatfile
      character*(MAX_LEN_FNAM) scatydatfile
      character*(MAX_LEN_FNAM) sstdatfile
      character*(MAX_LEN_FNAM) tmidatfile
      character*(MAX_LEN_FNAM) sssdatfile
      character*(MAX_LEN_FNAM) topexmeanfile
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
      character*(MAX_LEN_FNAM) udriftfile
      character*(MAX_LEN_FNAM) vdriftfile      
      character*(MAX_LEN_FNAM) curmtrufile
      character*(MAX_LEN_FNAM) curmtrvfile


c     Flags used in the model-data comparison:
c     ========================================
c
c     using_ers - flag that indicates the use of ERS data

      common /ecco_cost_data_flags/
     &                         using_topex,
     &                         using_ers,
     &                         using_gfo
      logical using_topex
      logical using_ers
      logical using_gfo

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
     &                           topexstartdate,
     &                           ersstartdate,
     &                           gfostartdate
      integer scatxstartdate(4)
      integer scatystartdate(4)
      integer sststartdate(4)
      integer argotstartdate(4)
      integer argosstartdate(4)
      integer tmistartdate(4)
      integer sssstartdate(4)
      integer topexstartdate(4)
      integer ersstartdate(4)
      integer gfostartdate(4)

      common /ecco_cost_data_aux_i/
     &                           tmistartdate1,
     &                           tmistartdate2,
     &                           sststartdate1,
     &                           sststartdate2,
     &                           sssstartdate1,
     &                           sssstartdate2,
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
     &                           scatstartdate2

      integer tmistartdate1
      integer tmistartdate2
      integer sststartdate1
      integer sststartdate2
      integer sssstartdate1
      integer sssstartdate2
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

c     ==================================================================
c     END OF HEADER COST
c     ==================================================================


