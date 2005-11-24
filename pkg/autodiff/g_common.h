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
     &                     g_gunm1, g_gvnm1, g_gtnm1, g_gsnm1
      _RL g_etan(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_gs(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gsnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gt(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gtnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gu(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gunm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gv(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_gvnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_salt(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_theta(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_uvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_vvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL g_wvel(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

      common /g_dynvars_r_2/
     &                     g_etah
      _RL g_etah(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

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

      common /g_ffields/
     &                   g_fu, g_fv
     &                 , g_qnet, g_empmr
cph     &                 , g_sst, g_sss
      _RL g_fu(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_fv(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_qnet(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_empmr(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
cph      _RL g_sst(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
cph      _RL g_sss(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

#ifdef ALLOW_DIFFKR_CONTROL
      COMMON /G_DYNVARS_DIFFKR/
     &                       g_diffKr
      _RL  g_diffKr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
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
      _RL g_lwflux    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_precip    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /g_exf_atm_temp_r/ g_atemp, g_aqh, g_lwflux, g_precip
# endif
# ifdef ALLOW_ATM_WIND
      _RL g_uwind     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_vwind     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /g_exf_atm_wind_r/ g_uwind, g_vwind
# endif
# ifdef ALLOW_DOWNWARD_RADIATION
      _RL g_swdown    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL g_lwdown    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /g_exf_rad_down_r/ g_swdown, g_lwdown
# endif
# ifdef ALLOW_CLIMSST_RELAXATION
      _RL g_climsst(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /adexf_clim_sst_r/ g_climsst
# endif
# ifdef ALLOW_CLIMSSS_RELAXATION
      _RL g_climsss(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /adexf_clim_sss_r/ g_climsss
# endif
#endif

#ifdef ALLOW_SEAICE
      _RL g_area(1-olx:snx+olx,1-oly:sny+oly,3,nsx,nsy)
      common /g_seaice_dynvars_1/ g_area
c
      _RL g_heff(1-olx:snx+olx,1-oly:sny+oly,3,nsx,nsy)
      _RL g_hsnow(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /g_seaice_trans/ g_heff, g_hsnow
# ifdef SEAICE_ALLOW_DYNAMICS
      _RL g_uice(1-olx:snx+olx,1-oly:sny+oly,3,nsx,nsy)
      _RL g_vice(1-olx:snx+olx,1-oly:sny+oly,3,nsx,nsy)
      common /g_seaice_dynvars_2/ g_uice, g_vice
# endif
#endif

#endif ALLOW_AUTODIFF_MONITOR
