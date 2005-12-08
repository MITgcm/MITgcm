C $Header: /u/gcmpack/MITgcm/pkg/autodiff/checkpoint_lev1_directives.h,v 1.16 2005/12/08 15:44:34 heimbach Exp $
C $Name:  $
c
c     store directives for checkpoint level 1
c
c     created: heimbach@mit.edu 10-Jan-2002
c
#ifdef NONLIN_FRSURF
CADJ STORE dEtaHdt       = comlev1, key = ikey_dynamics
CADJ STORE wVel          = comlev1, key = ikey_dynamics
CADJ STORE gUnm1         = comlev1, key = ikey_dynamics
CADJ STORE gVnm1         = comlev1, key = ikey_dynamics
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_CD_CODE
# include "cd_code_ad_check_lev1_dir.h"
#endif

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
CADJ STORE saltFlux0    = comlev1, key = ikey_dynamics
CADJ STORE saltFlux1    = comlev1, key = ikey_dynamics
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

#ifdef ALLOW_EBM
CADJ STORE fu      = comlev1, key = ikey_dynamics
CADJ STORE fv      = comlev1, key = ikey_dynamics
CADJ STORE sss     = comlev1, key = ikey_dynamics
CADJ STORE qnet    = comlev1, key = ikey_dynamics
CADJ STORE qsw     = comlev1, key = ikey_dynamics
# include "ebm_ad_check_lev1_dir.h"
#endif
