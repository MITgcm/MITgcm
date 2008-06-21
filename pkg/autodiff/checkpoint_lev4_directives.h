C $Header: /u/gcmpack/MITgcm/pkg/autodiff/checkpoint_lev4_directives.h,v 1.14 2008/06/21 13:49:08 heimbach Exp $
C $Name:  $
c
c     store directives for checkpoint level 4
c
c     created: heimbach@mit.edu 10-Jan-2002
c

CADJ STORE StoreDynVars3D     = tapelev4, key = ilev_4
CADJ STORE StoreDynVars2D     = tapelev4, key = ilev_4
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

