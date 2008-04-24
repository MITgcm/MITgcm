C $Header: /u/gcmpack/MITgcm/pkg/autodiff/checkpoint_lev1_directives.h,v 1.32 2008/04/24 21:39:57 gforget Exp $
C $Name:  $
c
c     store directives for checkpoint level 1
c
c     created: heimbach@mit.edu 10-Jan-2002
c
#ifdef NONLIN_FRSURF
c
CADJ STORE hfacc       = comlev1, key = ikey_dynamics
CADJ STORE hfac_surfc  = comlev1, key = ikey_dynamics
CADJ STORE recip_hfacc = comlev1, key = ikey_dynamics
CADJ STORE recip_hfacs = comlev1, key = ikey_dynamics
CADJ STORE recip_hfacw = comlev1, key = ikey_dynamics
cph the following are frequently needed, e.g. with seaice
CADJ STORE pmepr       = comlev1, key = ikey_dynamics
CADJ STORE totphihyd   = comlev1, key = ikey_dynamics
c
# ifndef DISABLE_RSTAR_CODE
CADJ STORE detahdt            = comlev1, key = ikey_dynamics
CADJ STORE gs,gsnm1,gt,gtnm1  = comlev1, key = ikey_dynamics
CADJ STORE salt,theta         = comlev1, key = ikey_dynamics
CADJ STORE uvel,vvel,wvel     = comlev1, key = ikey_dynamics
CADJ STORE h0facc,h0facs,h0facw 
CADJ &     = comlev1, key = ikey_dynamics
CADJ STORE rstarfacc,rstarfacs,rstarfacw
CADJ &     = comlev1, key = ikey_dynamics
CADJ STORE rstarexpc,rstarexps,rstarexpw
CADJ &     = comlev1, key = ikey_dynamics
# endif
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_DEPTH_CONTROL
CADJ STORE pmepr,surfaceforcingtice
CADJ &     = comlev1, key=ikey_dynamics
CADJ STORE detahdt
CADJ &     = comlev1, key=ikey_dynamics
CADJ STORE gs,gsnm1,gt,gtnm1,gunm1,gvnm1
CADJ &     = comlev1, key=ikey_dynamics
CADJ STORE theta,salt,totphihyd,wvel
CADJ &     = comlev1, key=ikey_dynamics
#endif /* ALLOW_DEPTH_CONTROL */

#ifdef ALLOW_CD_CODE
# include "cd_code_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_EXF
# include "exf_ad_check_lev1_dir.h"
#endif /* ALLOW_EXF undef */

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
# ifdef SHORTWAVE_HEATING
CADJ STORE Qsw0    = comlev1, key = ikey_dynamics
CADJ STORE Qsw1    = comlev1, key = ikey_dynamics
# endif
# ifdef ATMOSPHERIC_LOADING
CADJ STORE pload0  = comlev1, key = ikey_dynamics
CADJ STORE pload1  = comlev1, key = ikey_dynamics
CADJ STORE siceload = comlev1, key = ikey_dynamics
# endif

#ifdef ALLOW_OBCS
# include "obcs_ad_check_lev1_dir.h"
#endif  /* ALLOW_OBCS */

#ifdef ALLOW_SEAICE
# include "seaice_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_THSICE
# include "thsice_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_RBCS
# include "rbcs_ad_check_lev1_dir.h"
#endif /* ALLOW_RBCS */

#ifdef ALLOW_GCHEM
# include "gchem_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_CFC
# include "cfc_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_DIC
# include "dic_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_EBM
CADJ STORE fu      = comlev1, key = ikey_dynamics
CADJ STORE fv      = comlev1, key = ikey_dynamics
CADJ STORE sss     = comlev1, key = ikey_dynamics
CADJ STORE qnet    = comlev1, key = ikey_dynamics
CADJ STORE qsw     = comlev1, key = ikey_dynamics
CADJ STORE empmr   = comlev1, key = ikey_dynamics
CADJ STORE theta   = comlev1, key = ikey_dynamics
# include "ebm_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_COST
CADJ STORE cMeanThetaUVel = comlev1, key = ikey_dynamics
CADJ STORE cMeanThetaVVel = comlev1, key = ikey_dynamics
#endif

#ifdef ALLOW_COST_ATLANTIC
CADJ STORE theta = comlev1, key = ikey_dynamics
CADJ STORE uVel  = comlev1, key = ikey_dynamics
CADJ STORE vVel  = comlev1, key = ikey_dynamics
#endif
