c
c     store directives for checkpoint level 2
c
c     created: heimbach@mit.edu 10-Jan-2002
c
CADJ STORE gsnm1     = tapelev2, key = ilev_2
CADJ STORE gtnm1     = tapelev2, key = ilev_2
CADJ STORE gunm1     = tapelev2, key = ilev_2
CADJ STORE gvnm1     = tapelev2, key = ilev_2
CADJ STORE theta     = tapelev2, key = ilev_2
CADJ STORE salt      = tapelev2, key = ilev_2
CADJ STORE uvel      = tapelev2, key = ilev_2
CADJ STORE vvel      = tapelev2, key = ilev_2
CADJ STORE wvel      = tapelev2, key = ilev_2
CADJ STORE etan      = tapelev2, key = ilev_2
CADJ STORE totphihyd = tapelev2, key = ilev_2

#ifdef INCLUDE_CD_CODE
CADJ STORE uveld     = tapelev2, key = ilev_2
CADJ STORE vveld     = tapelev2, key = ilev_2
CADJ STORE etanm1    = tapelev2, key = ilev_2
CADJ STORE unm1      = tapelev2, key = ilev_2
CADJ STORE vnm1      = tapelev2, key = ilev_2
#endif /* INCLUDE_CD_CODE */

#ifdef EXACT_CONSERV
CADJ STORE empmr     = tapelev2, key = ilev_2
CADJ STORE etaH      = tapelev2, key = ilev_2
CADJ STORE hDivFlow  = tapelev2, key = ilev_2
#endif /* EXACT_CONSERV */

#ifdef ALLOW_PASSIVE_TRACER
CADJ STORE tr1       = tapelev2, key = ilev_2
CADJ STORE gtr1nm1   = tapelev2, key = ilev_2
#endif /* ALLOW_PASSIVE_TRACER */

#ifdef INCLUDE_EXTERNAL_FORCING_PACKAGE
# include "exf_ad_check_lev2_dir.h"
#else /* INCLUDE_EXTERNAL_FORCING_PACKAGE undef */

CADJ STORE taux0   = tapelev2, key = ilev_2
CADJ STORE taux1   = tapelev2, key = ilev_2
CADJ STORE tauy0   = tapelev2, key = ilev_2
CADJ STORE tauy1   = tapelev2, key = ilev_2
CADJ STORE Qnet0   = tapelev2, key = ilev_2
CADJ STORE Qnet1   = tapelev2, key = ilev_2
CADJ STORE EmPmR0  = tapelev2, key = ilev_2
CADJ STORE EmPmR1  = tapelev2, key = ilev_2
CADJ STORE SST0    = tapelev2, key = ilev_2
CADJ STORE SST1    = tapelev2, key = ilev_2
CADJ STORE SSS0    = tapelev2, key = ilev_2
CADJ STORE SSS1    = tapelev2, key = ilev_2
#ifdef SHORTWAVE_HEATING
CADJ STORE Qsw0    = tapelev2, key = ilev_2
CADJ STORE Qsw1    = tapelev2, key = ilev_2
#endif
#ifdef ATMOSPHERIC_LOADING
CADJ STORE pload0  = tapelev2, key = ilev_2
CADJ STORE pload1  = tapelev2, key = ilev_2
#endif

#endif /* INCLUDE_EXTERNAL_FORCING_PACKAGE */

#ifdef ALLOW_PTRACERS
# include "ptracers_ad_check_lev2_dir.h"
#endif /* ALLOW_PTRACERS */

#ifdef ALLOW_OBCS
# include "obcs_ad_check_lev2_dir.h"
#endif  /* ALLOW_OBCS */

#ifdef ALLOW_SEAICE
# include "seaice_ad_check_lev2_dir.h"
#endif /* ALLOW_SEAICE */

