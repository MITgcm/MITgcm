C $Header: /u/gcmpack/MITgcm/pkg/autodiff/checkpoint_lev2_directives.h,v 1.40 2008/06/21 13:49:08 heimbach Exp $
C $Name:  $
c
c     store directives for checkpoint level 4
c
c     created: heimbach@mit.edu 10-Jan-2002
c

CADJ STORE StoreDynVars3D     = tapelev2, key = ilev_2
CADJ STORE StoreDynVars2D     = tapelev2, key = ilev_2
cnewCADJ STORE ivdconvcount       = tapelevx, key = ilev_x

#ifdef EXACT_CONSERV
cphCADJ STORE hDivFlow  = tapelev2, key = ilev_2
#endif /* EXACT_CONSERV */

#ifdef ALLOW_PASSIVE_TRACER
CADJ STORE tr1       = tapelev2, key = ilev_2
CADJ STORE gtr1nm1   = tapelev2, key = ilev_2
#endif /* ALLOW_PASSIVE_TRACER */

#ifdef NONLIN_FRSURF
CADJ STORE hfac_surfc    = tapelev2, key = ilev_2
CADJ STORE hfac_surfs    = tapelev2, key = ilev_2
CADJ STORE hfac_surfw    = tapelev2, key = ilev_2
CADJ STORE hfacc         = tapelev2, key = ilev_2
CADJ STORE hfacs         = tapelev2, key = ilev_2
CADJ STORE hfacw         = tapelev2, key = ilev_2
CADJ STORE recip_hfacc   = tapelev2, key = ilev_2
CADJ STORE recip_hfacs   = tapelev2, key = ilev_2
CADJ STORE recip_hfacw   = tapelev2, key = ilev_2
# ifndef DISABLE_RSTAR_CODE
CADJ STORE rstarexpc,rstarexps,rstarexpw 
CADJ &     = tapelev2, key = ilev_2
CADJ STORE rstarfacc,rstarfacs,rstarfacw 
CADJ &     = tapelev2, key = ilev_2
CADJ STORE rstardhcdt,rstardhsdt,rstardhwdt 
CADJ &     = tapelev2, key = ilev_2
CADJ STORE h0facc,h0facs,h0facw
CADJ &     = tapelev2, key = ilev_2
# endif
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_CD_CODE
# include "cd_code_ad_check_lev2_dir.h"
#endif /* ALLOW_CD_CODE */

#ifdef ALLOW_EXF
# include "exf_ad_check_lev2_dir.h"
#endif /* ALLOW_EXF */

#ifdef ALLOW_PTRACERS
# include "ptracers_ad_check_lev2_dir.h"
#endif

#ifdef ALLOW_OBCS
# include "obcs_ad_check_lev2_dir.h"
#endif  /* ALLOW_OBCS */

#ifdef ALLOW_SEAICE
# include "seaice_ad_check_lev2_dir.h"
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_THSICE
# include "thsice_ad_check_lev2_dir.h"
#endif /* ALLOW_THSICE */

#ifdef ALLOW_RBCS
# include "rbcs_ad_check_lev2_dir.h"
#endif /* ALLOW_rbcs */

#ifdef ALLOW_GCHEM
# include "gchem_ad_check_lev2_dir.h"
#endif
 
#ifdef ALLOW_CFC
# include "cfc_ad_check_lev2_dir.h"
#endif
 
#ifdef ALLOW_DIC
# include "dic_ad_check_lev2_dir.h"
#endif

#ifdef ALLOW_EBM
CADJ STORE fu    = tapelev2, key = ilev_2
CADJ STORE fv    = tapelev2, key = ilev_2
CADJ STORE qnet  = tapelev2, key = ilev_2
CADJ STORE qsw   = tapelev2, key = ilev_2
CADJ STORE sss   = tapelev2, key = ilev_2
CADJ STORE empmr = tapelev2, key = ilev_2
# include "ebm_ad_check_lev2_dir.h"
#endif /* ALLOW_EBM */

#ifdef ALLOW_COST
CADJ STORE cMeanTheta = tapelev2, key = ilev_2
CADJ STORE cMeanUVel  = tapelev2, key = ilev_2
CADJ STORE cMeanVVel  = tapelev2, key = ilev_2
CADJ STORE cMeanThetaUVel = tapelev2, key = ilev_2
CADJ STORE cMeanThetaVVel = tapelev2, key = ilev_2
#endif

#ifdef ALLOW_COST_TRACER
CADJ STORE objf_tracer = tapelev2, key = ilev_2
#endif

#ifdef ALLOW_COST_TRANSPORT
CADJ STORE objf_transport = tapelev2, key = ilev_2
#endif

#ifdef ALLOW_HFLUXM_CONTROL
CADJ STORE qnetm          = tapelev2, key = ilev_2
#endif
