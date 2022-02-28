c
c     store directives for checkpoint level 4
c
c     created: heimbach@mit.edu 10-Jan-2002
c
#ifdef AUTODIFF_USE_STORE_RESTORE
c
CADJ STORE StoreDynVars2D = tapelev4, key = ilev_4
CADJ STORE StoreDynVars3D = tapelev4, key = ilev_4
c
#else
c
CADJ STORE etan  = tapelev4, key = ilev_4
#ifndef EXCLUDE_FFIELDS_LOAD
CADJ STORE taux0 = tapelev4, key = ilev_4
CADJ STORE taux1 = tapelev4, key = ilev_4
CADJ STORE tauy0 = tapelev4, key = ilev_4
CADJ STORE tauy1 = tapelev4, key = ilev_4
CADJ STORE qnet0 = tapelev4, key = ilev_4
CADJ STORE qnet1 = tapelev4, key = ilev_4
CADJ STORE empmr0 = tapelev4, key = ilev_4
CADJ STORE empmr1 = tapelev4, key = ilev_4
CADJ STORE sst0 = tapelev4, key = ilev_4
CADJ STORE sst1 = tapelev4, key = ilev_4
CADJ STORE sss0 = tapelev4, key = ilev_4
CADJ STORE sss1 = tapelev4, key = ilev_4
CADJ STORE saltflux0 = tapelev4, key = ilev_4
CADJ STORE saltflux1 = tapelev4, key = ilev_4
# ifdef SHORTWAVE_HEATING
CADJ STORE qsw0 = tapelev4, key = ilev_4
CADJ STORE qsw1 = tapelev4, key = ilev_4
# endif
# ifdef ALLOW_GEOTHERMAL_FLUX
CADJ STORE geothFlux0 = tapelev4, key = ilev_4
CADJ STORE geothFlux1 = tapelev4, key = ilev_4
# endif
# ifdef ATMOSPHERIC_LOADING
CADJ STORE pload0 = tapelev4, key = ilev_4
CADJ STORE pload1 = tapelev4, key = ilev_4
# endif
#endif /* ndef EXCLUDE_FFIELDS_LOAD */
#ifdef EXACT_CONSERV
CADJ STORE etaH = tapelev4, key = ilev_4
CADJ STORE dEtaHdt = tapelev4, key = ilev_4
CADJ STORE PmEpR = tapelev4, key = ilev_4
#endif
c
#ifdef ALLOW_ADAMSBASHFORTH_3
CADJ STORE gtnm = tapelev4, key = ilev_4
CADJ STORE gsnm = tapelev4, key = ilev_4
CADJ STORE gunm = tapelev4, key = ilev_4
CADJ STORE gvnm = tapelev4, key = ilev_4
#else
CADJ STORE gtnm1  = tapelev4, key = ilev_4
CADJ STORE gsnm1  = tapelev4, key = ilev_4
CADJ STORE gunm1  = tapelev4, key = ilev_4
CADJ STORE gvnm1  = tapelev4, key = ilev_4
#endif
CADJ STORE theta  = tapelev4, key = ilev_4
CADJ STORE salt  = tapelev4, key = ilev_4
CADJ STORE uvel  = tapelev4, key = ilev_4
CADJ STORE vvel  = tapelev4, key = ilev_4
CADJ STORE wvel  = tapelev4, key = ilev_4
CADJ STORE totphihyd  = tapelev4, key = ilev_4
c
#endif /* AUTODIFF_USE_STORE_RESTORE */

CADJ STORE phi0surf     = tapelev4, key = ilev_4
CADJ STORE saltflux     = tapelev4, key = ilev_4

#ifdef EXACT_CONSERV
cphCADJ STORE hDivFlow  = tapelev4, key = ilev_4
#endif /* EXACT_CONSERV */

#ifdef NONLIN_FRSURF
CADJ STORE hfac_surfc    = tapelev4, key = ilev_4
CADJ STORE hfac_surfs    = tapelev4, key = ilev_4
CADJ STORE hfac_surfw    = tapelev4, key = ilev_4
CADJ STORE hFac_surfNm1C= tapelev4, key = ilev_4
CADJ STORE hFac_surfNm1S= tapelev4, key = ilev_4
CADJ STORE hFac_surfNm1W= tapelev4, key = ilev_4

# ifndef DISABLE_RSTAR_CODE
CADJ STORE rstarexpc,rstarexps,rstarexpw
CADJ &     = tapelev4, key = ilev_4
CADJ STORE rstarfacc,rstarfacs,rstarfacw
CADJ &     = tapelev4, key = ilev_4
CADJ STORE rStarFacNm1C,rStarFacNm1S,rStarFacNm1W
CADJ &     = tapelev4, key = ilev_4
CADJ STORE rstardhcdt,rstardhsdt,rstardhwdt
CADJ &     = tapelev4, key = ilev_4
# endif

#endif /* NONLIN_FRSURF */

#if (defined ALLOW_CG2D_NSA || defined NONLIN_FRSURF || \
      defined ALLOW_DEPTH_CONTROL)
CADJ STORE aW2d, aS2d, aC2d = tapelev4, key = ilev_4
CADJ STORE pc, ps, pw       = tapelev4, key = ilev_4
#endif

#ifdef ALLOW_CD_CODE
# include "cd_code_ad_check_lev4_dir.h"
#endif /* ALLOW_CD_CODE */

#ifdef ALLOW_GGL90
# include "ggl90_ad_check_lev4_dir.h"
#endif

#ifdef ALLOW_ECCO
# include "ecco_ad_check_lev4_dir.h"
#endif

#ifdef ALLOW_EXF
# include "exf_ad_check_lev4_dir.h"
#endif /* ALLOW_EXF */

#ifdef ALLOW_CTRL
# include "ctrl_ad_check_lev4_dir.h"
#endif

#ifdef ALLOW_PTRACERS
# include "ptracers_ad_check_lev4_dir.h"
#endif

#ifdef ALLOW_OBCS
# include "obcs_ad_check_lev4_dir.h"
#endif  /* ALLOW_OBCS */

#ifdef ALLOW_SALT_PLUME
# include "salt_plume_ad_check_lev4_dir.h"
#endif

#ifdef ALLOW_SEAICE
CADJ STORE phiHydLow  = tapelev4, key = ilev_4
# include "seaice_ad_check_lev4_dir.h"
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_THSICE
# include "thsice_ad_check_lev4_dir.h"
#endif /* ALLOW_THSICE */

#ifdef ALLOW_SHELFICE
# include "shelfice_ad_check_lev4_dir.h"
#endif /* ALLOW_SHELFICE */

#ifdef ALLOW_STREAMICE
# include "streamice_ad_check_lev4_dir.h"
#endif /* ALLOW_STREAMICE */

#ifdef ALLOW_RBCS
# include "rbcs_ad_check_lev4_dir.h"
#endif /* ALLOW_RBCS */

#ifdef ALLOW_OFFLINE
# include "offline_ad_check_lev4_dir.h"
#endif /* ALLOW_OFFLINE */

#ifdef ALLOW_CFC
# include "cfc_ad_check_lev4_dir.h"
#endif

#ifdef ALLOW_DIC
# include "dic_ad_check_lev4_dir.h"
#endif

#ifdef ALLOW_BLING
# include "bling_ad_check_lev4_dir.h"
#endif

#ifdef ALLOW_GENERIC_ADVDIFF
# include "gad_ad_check_lev4_dir.h"
#endif

#ifdef ALLOW_EBM
CADJ STORE fu    = tapelev4, key = ilev_4
CADJ STORE fv    = tapelev4, key = ilev_4
CADJ STORE qnet  = tapelev4, key = ilev_4
CADJ STORE qsw   = tapelev4, key = ilev_4
CADJ STORE sss   = tapelev4, key = ilev_4
CADJ STORE empmr = tapelev4, key = ilev_4
# include "ebm_ad_check_lev4_dir.h"
#endif /* ALLOW_EBM */

#ifdef ALLOW_COST
CADJ STORE cMeanTheta = tapelev4, key = ilev_4
CADJ STORE cMeanUVel  = tapelev4, key = ilev_4
CADJ STORE cMeanVVel  = tapelev4, key = ilev_4
CADJ STORE cMeanThetaUVel = tapelev4, key = ilev_4
CADJ STORE cMeanThetaVVel = tapelev4, key = ilev_4
#endif

#ifdef ALLOW_COST_TRACER
CADJ STORE objf_tracer = tapelev4, key = ilev_4
#endif

#ifdef ALLOW_COST_TRANSPORT
CADJ STORE objf_transport = tapelev4, key = ilev_4
#endif

#ifdef ALLOW_HFLUXM_CONTROL
CADJ STORE qnetm          = tapelev4, key = ilev_4
#endif

#ifdef ALLOW_SEAICE
cph temporary for HD
# ifdef ANNUAL_BALANCE
CADJ STORE balance_itcount = tapelev4, key = ilev_4
CADJ STORE atmfw_tilesum   = tapelev4, key = ilev_4
CADJ STORE qnet_tilesum    = tapelev4, key = ilev_4
CADJ STORE empmr_corr      = tapelev4, key = ilev_4
CADJ STORE qnet_corr       = tapelev4, key = ilev_4
# endif /* ANNUAL_BALANCE */
#endif
