C $Header: /u/gcmpack/MITgcm/pkg/autodiff/checkpoint_lev4_directives.h,v 1.16 2008/10/27 20:45:01 heimbach Exp $
C $Name:  $
c
c     store directives for checkpoint level 4
c
c     created: heimbach@mit.edu 10-Jan-2002
c
#ifdef AUTODIFF_USE_OLDSTORE_2D
c
CADJ STORE etan  = tapelev4, key = ilev_4
CADJ STORE surfaceforcingTice = tapelev4, key = ilev_4
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
#ifdef SHORTWAVE_HEATING
CADJ STORE qsw0 = tapelev4, key = ilev_4
CADJ STORE qsw1 = tapelev4, key = ilev_4
#endif
#ifdef ATMOSPHERIC_LOADING
CADJ STORE pload0 = tapelev4, key = ilev_4
CADJ STORE pload1 = tapelev4, key = ilev_4
#endif
#ifdef EXACT_CONSERV
CADJ STORE etaH = tapelev4, key = ilev_4
CADJ STORE dEtaHdt = tapelev4, key = ilev_4
CADJ STORE PmEpR = tapelev4, key = ilev_4
#endif
c
#else /* ndef AUTODIFF_USE_OLDSTORE_2D */
c
CADJ STORE StoreDynVars2D     = tapelev4, key = ilev_4
c
#endif /* AUTODIFF_USE_OLDSTORE_2D */
c
#ifdef AUTODIFF_USE_OLDSTORE_3D
c
CADJ STORE gs  = tapelev4, key = ilev_4
CADJ STORE gt  = tapelev4, key = ilev_4             
CADJ STORE gtnm1  = tapelev4, key = ilev_4             
CADJ STORE gsnm1  = tapelev4, key = ilev_4             
CADJ STORE gunm1  = tapelev4, key = ilev_4             
CADJ STORE gvnm1  = tapelev4, key = ilev_4             
CADJ STORE theta  = tapelev4, key = ilev_4             
CADJ STORE salt  = tapelev4, key = ilev_4             
CADJ STORE uvel  = tapelev4, key = ilev_4             
CADJ STORE vvel  = tapelev4, key = ilev_4     
CADJ STORE wvel  = tapelev4, key = ilev_4
CADJ STORE totphihyd  = tapelev4, key = ilev_4
c
#else /* ndef AUTODIFF_USE_OLDSTORE_3D */
c
CADJ STORE StoreDynVars3D     = tapelev4, key = ilev_4
c
#endif /* AUTODIFF_USE_OLDSTORE_3D */

cnewCADJ STORE ivdconvcount       = tapelevx, key = ilev_x

#ifdef EXACT_CONSERV
cphCADJ STORE hDivFlow  = tapelev4, key = ilev_4
#endif /* EXACT_CONSERV */

#ifdef ALLOW_PASSIVE_TRACER
CADJ STORE tr1       = tapelev4, key = ilev_4
CADJ STORE gtr1nm1   = tapelev4, key = ilev_4
#endif /* ALLOW_PASSIVE_TRACER */

#ifdef NONLIN_FRSURF
CADJ STORE hfac_surfc    = tapelev4, key = ilev_4
CADJ STORE hfac_surfs    = tapelev4, key = ilev_4
CADJ STORE hfac_surfw    = tapelev4, key = ilev_4
CADJ STORE hfacc         = tapelev4, key = ilev_4
CADJ STORE hfacs         = tapelev4, key = ilev_4
CADJ STORE hfacw         = tapelev4, key = ilev_4
CADJ STORE recip_hfacc   = tapelev4, key = ilev_4
CADJ STORE recip_hfacs   = tapelev4, key = ilev_4
CADJ STORE recip_hfacw   = tapelev4, key = ilev_4
# ifndef DISABLE_RSTAR_CODE
CADJ STORE rstarexpc,rstarexps,rstarexpw 
CADJ &     = tapelev4, key = ilev_4
CADJ STORE rstarfacc,rstarfacs,rstarfacw 
CADJ &     = tapelev4, key = ilev_4
CADJ STORE rstardhcdt,rstardhsdt,rstardhwdt 
CADJ &     = tapelev4, key = ilev_4
CADJ STORE h0facc,h0facs,h0facw
CADJ &     = tapelev4, key = ilev_4
# endif
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_CD_CODE
# include "cd_code_ad_check_lev4_dir.h"
#endif /* ALLOW_CD_CODE */

#ifdef ALLOW_EXF
# include "exf_ad_check_lev4_dir.h"
#endif /* ALLOW_EXF */

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
# include "seaice_ad_check_lev4_dir.h"
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_THSICE
# include "thsice_ad_check_lev4_dir.h"
#endif /* ALLOW_THSICE */

#ifdef ALLOW_RBCS
# include "rbcs_ad_check_lev4_dir.h"
#endif /* ALLOW_rbcs */

#ifdef ALLOW_GCHEM
# include "gchem_ad_check_lev4_dir.h"
#endif
 
#ifdef ALLOW_CFC
# include "cfc_ad_check_lev4_dir.h"
#endif
 
#ifdef ALLOW_DIC
# include "dic_ad_check_lev4_dir.h"
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

