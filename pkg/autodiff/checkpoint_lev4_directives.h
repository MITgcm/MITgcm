C $Header: /u/gcmpack/MITgcm/pkg/autodiff/checkpoint_lev4_directives.h,v 1.1 2005/08/10 03:34:48 heimbach Exp $
C $Name:  $
c
c     store directives for checkpoint level 4
c
c     created: heimbach@mit.edu 10-Jan-2002
c
CADJ STORE gs        = tapelev4, key = ilev_4
CADJ STORE gt        = tapelev4, key = ilev_4
CADJ STORE gtnm1     = tapelev4, key = ilev_4
CADJ STORE gsnm1     = tapelev4, key = ilev_4
CADJ STORE gunm1     = tapelev4, key = ilev_4
CADJ STORE gvnm1     = tapelev4, key = ilev_4
CADJ STORE theta     = tapelev4, key = ilev_4
CADJ STORE salt      = tapelev4, key = ilev_4
CADJ STORE uvel      = tapelev4, key = ilev_4
CADJ STORE vvel      = tapelev4, key = ilev_4
CADJ STORE wvel      = tapelev4, key = ilev_4
CADJ STORE etan      = tapelev4, key = ilev_4
CADJ STORE totphihyd = tapelev4, key = ilev_4
CADJ STORE surfaceforcingTice = tapelev4, key = ilev_4
cnewCADJ STORE ivdconvcount       = tapelev4, key = ilev_4

#ifdef EXACT_CONSERV
CADJ STORE etaH      = tapelev4, key = ilev_4
CADJ STORE dEtaHdt   = tapelev4, key = ilev_4
CADJ STORE PmEpR     = tapelev4, key = ilev_4
cphCADJ STORE hDivFlow  = tapelev4, key = ilev_4
#endif /* EXACT_CONSERV */

#ifdef ALLOW_PASSIVE_TRACER
CADJ STORE tr1       = tapelev4, key = ilev_4
CADJ STORE gtr1nm1   = tapelev4, key = ilev_4
#endif /* ALLOW_PASSIVE_TRACER */

#ifdef ALLOW_CD_CODE
# include "cd_code_ad_check_lev4_dir.h"
#endif /* ALLOW_CD_CODE */

#ifdef ALLOW_EXF
# include "exf_ad_check_lev4_dir.h"
#else /* ALLOW_EXF undef */

CADJ STORE taux0   = tapelev4, key = ilev_4
CADJ STORE taux1   = tapelev4, key = ilev_4
CADJ STORE tauy0   = tapelev4, key = ilev_4
CADJ STORE tauy1   = tapelev4, key = ilev_4
CADJ STORE Qnet0   = tapelev4, key = ilev_4
CADJ STORE Qnet1   = tapelev4, key = ilev_4
CADJ STORE EmPmR0  = tapelev4, key = ilev_4
CADJ STORE EmPmR1  = tapelev4, key = ilev_4
CADJ STORE SST0    = tapelev4, key = ilev_4
CADJ STORE SST1    = tapelev4, key = ilev_4
CADJ STORE SSS0    = tapelev4, key = ilev_4
CADJ STORE SSS1    = tapelev4, key = ilev_4
CADJ STORE saltFlux0    = tapelev4, key = ilev_4
CADJ STORE saltFlux1    = tapelev4, key = ilev_4
#ifdef SHORTWAVE_HEATING
CADJ STORE Qsw0    = tapelev4, key = ilev_4
CADJ STORE Qsw1    = tapelev4, key = ilev_4
#endif
#ifdef ATMOSPHERIC_LOADING
CADJ STORE pload0  = tapelev4, key = ilev_4
CADJ STORE pload1  = tapelev4, key = ilev_4
#endif

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

#ifdef ALLOW_EBM
CADJ STORE fu    = tapelev4, key = ilev_4
CADJ STORE fv    = tapelev4, key = ilev_4
CADJ STORE qnet  = tapelev4, key = ilev_4
CADJ STORE qsw   = tapelev4, key = ilev_4
CADJ STORE sss   = tapelev4, key = ilev_4
CADJ STORE empmr = tapelev4, key = ilev_4
# include "ebm_ad_check_lev4_dir.h"
#endif /* ALLOW_EBM */

#ifdef ALLOW_COST_ATLANTIC_HEAT
CADJ STORE cMeanTheta = tapelev4, key = ilev_4
CADJ STORE cMeanUVel  = tapelev4, key = ilev_4
CADJ STORE cMeanVVel  = tapelev4, key = ilev_4
#endif

#ifdef ALLOW_COST_TRACER
CADJ STORE objf_tracer = tapelev4, key = ilev_4
#endif
