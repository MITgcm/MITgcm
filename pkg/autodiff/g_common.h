C--   These common blocks are extracted from the
C--   automatically created tangent linear code.
C--   You need to make sure that they are up-to-date
C--   (i.e. in right order), and customize them
C--   accordingly.
C--
C--   heimbach@mit.edu 11-Jan-2001

#ifdef ALLOW_AUTODIFF_MONITOR

#include "PACKAGES_CONFIG.h"

#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif

      common /g_dynvars_r/
     &                     g_etan,
     &                     g_uvel, g_vvel, g_wvel,
     &                     g_theta, g_salt,
     &                     g_gu, g_gv, g_gt, g_gs,
#ifdef ALLOW_ADAMSBASHFORTH_3
     &                     g_gunm, g_gvnm, g_gtnm, g_gsnm
#else
     &                     g_gunm1, g_gvnm1, g_gtnm1, g_gsnm1
#endif
      _RL g_etan(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_gs(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gt(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gu(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gv(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_salt(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_theta(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_uvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_vvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_wvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#ifdef ALLOW_ADAMSBASHFORTH_3
      _RL g_gtnm(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy,2)
      _RL g_gsnm(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy,2)
      _RL g_gunm(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy,2)
      _RL g_gvnm(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy,2)
#else
      _RL g_gtnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gsnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gunm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gvnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif

      common /g_dynvars_r_2/
     &                     g_etah
      _RL g_etah(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

#ifdef ALLOW_AUTODIFF_MONITOR_DIAG
      common /g_dynvars_diag/
     &                     g_totphihyd, g_rhoinsitu
      _RL g_rhoinsitu(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_totphihyd(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif

#ifdef ALLOW_CD_CODE
      common /g_dynvars_cd/
     &                      g_uveld, g_vveld,
     &                      g_etanm1,
     &                      g_unm1, g_vnm1
      _RL g_uveld(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_vveld(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_etanm1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_unm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_vnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif

#ifdef ALLOW_AUTODIFF_MONITOR_PHIHYD
cph this is a tricky one since TAF exctracts it from a
cph larger common block, so be careful
      common /g_dynvars_diag/ g_totphihyd
      _RL g_totphihyd(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
#endif

      COMMON /g_ffields_fu/ g_fu
      COMMON /g_ffields_fv/ g_fv
      COMMON /g_ffields_Qnet/ g_Qnet
      COMMON /g_ffields_Qsw/ g_Qsw
      COMMON /g_ffields_EmPmR/ g_EmPmR
      COMMON /g_ffields_saltFlux/ g_saltFlux
      COMMON /g_ffields_SST/ g_SST
      COMMON /g_ffields_SSS/ g_SSS
      COMMON /g_ffields_lambdaThetaClimRelax/ g_lambdaThetaClimRelax
      COMMON /g_ffields_lambdaSaltClimRelax/ g_lambdaSaltClimRelax
      _RS  g_fu       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_fv       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_Qnet     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_Qsw      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_EmPmR    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_saltFlux (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_SST      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_SSS      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_lambdaThetaClimRelax
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_lambdaSaltClimRelax
     &    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ATMOSPHERIC_LOADING
      COMMON /g_ffields_pload/ g_pload
      COMMON /g_ffields_sIceLoad/ g_sIceLoad
      _RS  g_pload    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  g_sIceLoad (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef ALLOW_DIFFKR_CONTROL
      COMMON /G_DYNVARS_DIFFKR/
     &                       g_diffKr
      _RL  g_diffKr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#ifdef ALLOW_KAPGM_CONTROL
      COMMON /G_CTRL_FIELDS_KAPGM/
     &                       g_kapgm
      _RL  g_kapgm (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#ifdef ALLOW_KAPREDI_CONTROL
      COMMON /G_CTRL_FIELDS_KAPREDI/
     &                       g_kapredi
      _RL  g_kapredi (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#ifdef ALLOW_BOTTOMDRAG_CONTROL
      COMMON /G_CTRL_FIELDS_BOTTOMDRAG/
     &                       g_bottomdragfld
      _RL  g_bottomdragfld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#ifdef ALLOW_EXF
      _RL g_hflux(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_sflux(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /g_exf_hsflux_r/ g_hflux, g_sflux
      _RL g_ustress(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_vstress(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /g_exf_stress_r/ g_ustress, g_vstress
# ifdef ALLOW_ATM_TEMP
      _RL g_atemp     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_aqh       (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_hs        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_hl        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_lwflux    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_evap      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_precip    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_snowprecip(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /g_exf_atm_temp_r/ g_atemp, g_aqh, g_hs, g_hl,
     &  g_lwflux, g_evap, g_precip, g_snowprecip
# endif /* ALLOW_ATM_TEMP */

      _RL g_uwind     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_vwind     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /g_exf_atm_wind_r/ g_uwind, g_vwind

# ifdef ALLOW_DOWNWARD_RADIATION
      _RL g_swdown    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_lwdown    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /g_exf_rad_down_r/ g_swdown, g_lwdown
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
      _RL g_climsst(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /g_exf_clim_sst_r/ g_climsst
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
      _RL g_climsss(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /g_exf_clim_sss_r/ g_climsss
# endif
#endif /* ALLOW_EXF */

#ifdef ALLOW_SEAICE
      _RL g_area  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_heff  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_hsnow (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_uice  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_vice  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /g_seaice_dynvars_1/
     &     g_area, g_heff, g_hsnow, g_uice, g_vice
#endif

#ifdef ALLOW_DEPTH_CONTROL
      _RS g_r_low_control(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS g_hfacc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      _RS g_hfacs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      _RS g_hfacw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      _RS g_recip_rcol(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS g_recip_hfacc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      _RS g_recip_hfacs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      _RS g_recip_hfacw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:nr,nSx,nSy)
      common /g_grid_var_rs/
     &     g_hfacc, g_hfacw, g_hfacs,
     &     g_recip_hfacc, g_recip_hfacw, g_recip_hfacs,
     &     g_r_low_control, g_recip_rcol
#endif /* ALLOW_DEPTH_CONTROL */

#endif /* ALLOW_AUTODIFF_MONITOR */
