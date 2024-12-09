C
C     store directives for checkpoint level 1
C
C     created: heimbach@mit.edu 10-Jan-2002
C

CADJ STORE totPhiHyd = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE phi0surf  = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE saltFlux  = comlev1, key = ikey_dynamics, kind = isbyte
#ifdef ATMOSPHERIC_LOADING
CADJ STORE sIceLoad  = comlev1, key = ikey_dynamics, kind = isbyte
#endif
#ifdef EXACT_CONSERV
CADJ STORE PmEpR     = comlev1, key = ikey_dynamics,  kind = isbyte
CADJ STORE dEtaHdt   = comlev1, key = ikey_dynamics,  kind = isbyte
#endif
CADJ STORE wVel      = comlev1, key = ikey_dynamics, kind = isbyte

#ifdef ALLOW_ADAMSBASHFORTH_3
CADJ STORE gtNm, gsNm   = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE guNm, gvNm   = comlev1, key = ikey_dynamics, kind = isbyte
#else
CADJ STORE gtNm1, gsNm1 = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE guNm1, gvNm1 = comlev1, key = ikey_dynamics, kind = isbyte
#endif

#if (defined ALLOW_CG2D_NSA || defined NONLIN_FRSURF || \
      defined ALLOW_DEPTH_CONTROL)
CADJ STORE aW2d,aS2d,aC2d= comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE pW, pS, pC    = comlev1, key = ikey_dynamics, kind = isbyte
#endif

#ifdef NONLIN_FRSURF

CADJ STORE hFac_surfC    = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE hFac_surfW    = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE hFac_surfS    = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE hfac_surfNm1C = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE hfac_surfNm1W = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE hfac_surfNm1S = comlev1, key = ikey_dynamics, kind = isbyte

CADJ STORE theta, salt   = comlev1, key = ikey_dynamics,kind = isbyte
CADJ STORE uVel, vVel    = comlev1, key = ikey_dynamics,kind = isbyte

# ifndef DISABLE_RSTAR_CODE
CADJ STORE rStarFacC = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE rStarFacNm1C, rStarFacNm1W, rStarFacNm1S = comlev1,
CADJ &     key = ikey_dynamics,kind = isbyte
CADJ STORE rStarExpC, rStarExpW, rStarExpS = comlev1,
CADJ &     key = ikey_dynamics,kind = isbyte
CADJ STORE rStarDhCDt, rStarDhWDt, rStarDhSDt = comlev1,
CADJ &     key = ikey_dynamics,kind = isbyte
# endif

#else /* NONLIN_FRSURF */

CADJ STORE etaH  = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE salt  = comlev1, key = ikey_dynamics, kind = isbyte
# if ( defined ALLOW_ECCO || defined ALLOW_SEAICE \
    || defined ALLOW_DEPTH_CONTROL )
CADJ STORE theta = comlev1, key = ikey_dynamics,kind = isbyte
# endif /* ALLOW_ECCO or ALLOW_SEAICE or ALLOW_DEPTH_CONTROL */

#endif /* NONLIN_FRSURF */

#ifdef ALLOW_CD_CODE
# include "cd_code_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_GGL90
# include "ggl90_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_EXF
# include "exf_ad_check_lev1_dir.h"
#endif /* ALLOW_EXF undef */

#ifdef ALLOW_CTRL
# include "ctrl_ad_check_lev1_dir.h"
#endif

#ifndef EXCLUDE_FFIELDS_LOAD
# ifdef STORE_LOADEDREC_TEST
CADJ STORE loadedRec = comlev1, key = ikey_dynamics, kind = 4
# endif
CADJ STORE taux0     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE taux1     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE tauy0     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE tauy1     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE Qnet0     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE Qnet1     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE EmPmR0    = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE EmPmR1    = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE SST0      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE SST1      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE SSS0      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE SSS1      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE saltFlux0 = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE saltFlux1 = comlev1, key = ikey_dynamics, kind = isbyte
# ifdef SHORTWAVE_HEATING
CADJ STORE Qsw0      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE Qsw1      = comlev1, key = ikey_dynamics, kind = isbyte
# endif
# ifdef ALLOW_GEOTHERMAL_FLUX
CADJ STORE geothFlux0 = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE geothFlux1 = comlev1, key = ikey_dynamics, kind = isbyte
# endif
# ifdef ATMOSPHERIC_LOADING
CADJ STORE pLoad0    = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE pLoad1    = comlev1, key = ikey_dynamics, kind = isbyte
# endif
#endif /* ndef EXCLUDE_FFIELDS_LOAD */

#ifdef ALLOW_OBCS
# include "obcs_ad_check_lev1_dir.h"
#endif  /* ALLOW_OBCS */

#ifdef ALLOW_SALT_PLUME
# include "salt_plume_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_SEAICE
CADJ STORE phiHydLow = comlev1, key = ikey_dynamics, kind = isbyte
# include "seaice_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_THSICE
# include "thsice_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_SHELFICE
# include "shelfice_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_RBCS
# include "rbcs_ad_check_lev1_dir.h"
#endif /* ALLOW_RBCS */

#ifdef ALLOW_OFFLINE
# include "offline_ad_check_lev1_dir.h"
#endif /* ALLOW_OFFLINE */

#ifdef ALLOW_PTRACERS
# include "ptracers_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_CFC
# include "cfc_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_DIC
# include "dic_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_BLING
# include "bling_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_GENERIC_ADVDIFF
# include "gad_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_EBM
CADJ STORE fu      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE fv      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE SSS     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE Qnet    = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE Qsw     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE EmPmR   = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE theta   = comlev1, key = ikey_dynamics, kind = isbyte
# include "ebm_ad_check_lev1_dir.h"
#endif

#ifdef ALLOW_COST
CADJ STORE cMeanThetaUVel = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE cMeanThetaVVel = comlev1, key = ikey_dynamics, kind = isbyte
#endif

#ifdef ALLOW_COST_ATLANTIC_HEAT
CADJ STORE theta = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE uVel  = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE vVel  = comlev1, key = ikey_dynamics, kind = isbyte
#endif

#ifdef ALLOW_SEAICE
# ifdef ALLOW_DOWN_SLOPE
CADJ STORE ETA, ZETA = comlev1, key = ikey_dynamics, kind = isbyte
# endif
#endif
