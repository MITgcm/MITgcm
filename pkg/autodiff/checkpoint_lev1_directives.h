c
c     store directives for checkpoint level 1
c
c     created: heimbach@mit.edu 10-Jan-2002
c
CADJ STORE surfacetendencyTice = comlev1, key = ikey_dynamics

#ifdef ALLOW_EXF
# include "exf_ad_check_lev1_dir.h"
#else /* ALLOW_EXF undef */

CADJ STORE taux0   = comlev1, key = ikey_dynamics
CADJ STORE taux1   = comlev1, key = ikey_dynamics
CADJ STORE tauy0   = comlev1, key = ikey_dynamics
CADJ STORE tauy1   = comlev1, key = ikey_dynamics
CADJ STORE Qnet0   = comlev1, key = ikey_dynamics
CADJ STORE Qnet1   = comlev1, key = ikey_dynamics
CADJ STORE EmPmR0  = comlev1, key = ikey_dynamics
CADJ STORE EmPmR1  = comlev1, key = ikey_dynamics
CADJ STORE SST0    = comlev1, key = ikey_dynamics
CADJ STORE SST1    = comlev1, key = ikey_dynamics
CADJ STORE SSS0    = comlev1, key = ikey_dynamics
CADJ STORE SSS1    = comlev1, key = ikey_dynamics
#ifdef SHORTWAVE_HEATING
CADJ STORE Qsw0    = comlev1, key = ikey_dynamics
CADJ STORE Qsw1    = comlev1, key = ikey_dynamics
#endif
#ifdef ATMOSPHERIC_LOADING
CADJ STORE pload0  = comlev1, key = ikey_dynamics
CADJ STORE pload1  = comlev1, key = ikey_dynamics
#endif

#endif /* ALLOW_EXF */

#ifdef ALLOW_OBCS
# include "exf_ad_check_lev1_dir.h"
#endif  /* ALLOW_OBCS */

#ifdef ALLOW_SEAICE
# include "seaice_ad_check_lev1_dir.h"
#endif

