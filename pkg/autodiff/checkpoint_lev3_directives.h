C $Header: /u/gcmpack/MITgcm/pkg/autodiff/checkpoint_lev3_directives.h,v 1.29 2006/05/03 23:35:38 heimbach Exp $
C $Name:  $
c
c     store directives for checkpoint level 3
c
c     created: heimbach@mit.edu 10-Jan-2002
c
CADJ STORE gs        = tapelev3, key = ilev_3
CADJ STORE gt        = tapelev3, key = ilev_3
CADJ STORE gtnm1     = tapelev3, key = ilev_3
CADJ STORE gsnm1     = tapelev3, key = ilev_3
CADJ STORE gunm1     = tapelev3, key = ilev_3
CADJ STORE gvnm1     = tapelev3, key = ilev_3
CADJ STORE theta     = tapelev3, key = ilev_3
CADJ STORE salt      = tapelev3, key = ilev_3
CADJ STORE uvel      = tapelev3, key = ilev_3
CADJ STORE vvel      = tapelev3, key = ilev_3
CADJ STORE wvel      = tapelev3, key = ilev_3
CADJ STORE etan      = tapelev3, key = ilev_3
CADJ STORE totphihyd = tapelev3, key = ilev_3
CADJ STORE surfaceforcingTice = tapelev3, key = ilev_3
cnewCADJ STORE ivdconvcount       = tapelev3, key = ilev_3

#ifdef EXACT_CONSERV
CADJ STORE etaH      = tapelev3, key = ilev_3
CADJ STORE dEtaHdt   = tapelev3, key = ilev_3
CADJ STORE PmEpR     = tapelev3, key = ilev_3
cphCADJ STORE hDivFlow  = tapelev3, key = ilev_3
#endif /* EXACT_CONSERV */

#ifdef ALLOW_PASSIVE_TRACER
CADJ STORE tr1       = tapelev3, key = ilev_3
CADJ STORE gtr1nm1   = tapelev3, key = ilev_3
#endif /* ALLOW_PASSIVE_TRACER */

#ifdef NONLIN_FRSURF
CADJ STORE hfac_surfc    = tapelev3, key = ilev_3
CADJ STORE hfac_surfs    = tapelev3, key = ilev_3
CADJ STORE hfac_surfw    = tapelev3, key = ilev_3
CADJ STORE hfacc         = tapelev3, key = ilev_3
CADJ STORE hfacs         = tapelev3, key = ilev_3
CADJ STORE hfacw         = tapelev3, key = ilev_3
CADJ STORE recip_hfacc   = tapelev3, key = ilev_3
CADJ STORE recip_hfacs   = tapelev3, key = ilev_3
CADJ STORE recip_hfacw   = tapelev3, key = ilev_3
# ifndef DISABLE_RSTAR_CODE
CADJ STORE rstarexpc,rstarexps,rstarexpw 
CADJ &     = tapelev3, key = ilev_3
CADJ STORE rstarfacc,rstarfacs,rstarfacw 
CADJ &     = tapelev3, key = ilev_3
CADJ STORE rstardhcdt,rstardhsdt,rstardhwdt 
CADJ &     = tapelev3, key = ilev_3
CADJ STORE h0facc,h0facs,h0facw
CADJ &     = tapelev3, key = ilev_3
# endif
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_CD_CODE
# include "cd_code_ad_check_lev3_dir.h"
#endif /* ALLOW_CD_CODE */

#ifdef ALLOW_EXF
# include "exf_ad_check_lev3_dir.h"
#else /* ALLOW_EXF undef */

CADJ STORE taux0   = tapelev3, key = ilev_3
CADJ STORE taux1   = tapelev3, key = ilev_3
CADJ STORE tauy0   = tapelev3, key = ilev_3
CADJ STORE tauy1   = tapelev3, key = ilev_3
CADJ STORE Qnet0   = tapelev3, key = ilev_3
CADJ STORE Qnet1   = tapelev3, key = ilev_3
CADJ STORE EmPmR0  = tapelev3, key = ilev_3
CADJ STORE EmPmR1  = tapelev3, key = ilev_3
CADJ STORE SST0    = tapelev3, key = ilev_3
CADJ STORE SST1    = tapelev3, key = ilev_3
CADJ STORE SSS0    = tapelev3, key = ilev_3
CADJ STORE SSS1    = tapelev3, key = ilev_3
CADJ STORE saltFlux0    = tapelev3, key = ilev_3
CADJ STORE saltFlux1    = tapelev3, key = ilev_3
#ifdef SHORTWAVE_HEATING
CADJ STORE Qsw0    = tapelev3, key = ilev_3
CADJ STORE Qsw1    = tapelev3, key = ilev_3
#endif
#ifdef ATMOSPHERIC_LOADING
CADJ STORE pload0  = tapelev3, key = ilev_3
CADJ STORE pload1  = tapelev3, key = ilev_3
#endif

#endif /* ALLOW_EXF */

#ifdef ALLOW_PTRACERS
# include "ptracers_ad_check_lev3_dir.h"
#endif

#ifdef ALLOW_OBCS
# include "obcs_ad_check_lev3_dir.h"
#endif  /* ALLOW_OBCS */

#ifdef ALLOW_SEAICE
# include "seaice_ad_check_lev3_dir.h"
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_THSICE
# include "thsice_ad_check_lev3_dir.h"
#endif /* ALLOW_SEAICE */

#ifdef ALLOW_EBM
CADJ STORE fu    = tapelev3, key = ilev_3
CADJ STORE fv    = tapelev3, key = ilev_3
CADJ STORE qnet  = tapelev3, key = ilev_3
CADJ STORE qsw   = tapelev3, key = ilev_3
CADJ STORE sss   = tapelev3, key = ilev_3
CADJ STORE empmr = tapelev3, key = ilev_3
# include "ebm_ad_check_lev3_dir.h"
#endif /* ALLOW_EBM */

#if (defined (ALLOW_COST_TEST) || defined (ALLOW_COST_ATLANTIC_HEAT))
CADJ STORE cMeanTheta = tapelev3, key = ilev_3
CADJ STORE cMeanUVel  = tapelev3, key = ilev_3
CADJ STORE cMeanVVel  = tapelev3, key = ilev_3
#endif

#ifdef ALLOW_COST_TRACER
CADJ STORE objf_tracer = tapelev3, key = ilev_3
#endif
