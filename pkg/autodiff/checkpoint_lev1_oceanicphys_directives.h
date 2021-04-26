C
C     Store directives for checkpoint level 1 AFTER do_oceanic_phys
C     has been called
C
C     This block of store directives is for variables that have been
C     computed in do_oceanics_phys. Storing them here avoids calling
C     do_oceanic_phys again in forward_step_ad, which should improve
C     performance because expensive parts of the model are not
C     recomputed (e.g. seaice).
C
CADJ STORE EmPmR          = comlev1, key = ikey_dynamics, kind = isbyte
# ifdef EXACT_CONSERV
CADJ STORE PmEpR          = comlev1, key = ikey_dynamics, kind = isbyte
# endif
CADJ STORE qsw            = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE surfaceForcingU= comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE surfaceForcingV= comlev1, key = ikey_dynamics, kind = isbyte
# ifdef ATMOSPHERIC_LOADING
CADJ STORE phi0surf       = comlev1, key = ikey_dynamics, kind = isbyte
# endif

# ifdef ALLOW_OBCS
#  include "obcs_ad_check_lev1_dir.h"
CADJ STORE salt, theta    = comlev1, key = ikey_dynamics, kind = isbyte
# endif /* ALLOW_OBCS */

# ifdef ALLOW_DEPTH_CONTROL
CADJ STORE hFacC, hFacS, hFacW
CADJ &     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE recip_hFacC, recip_hFacS, recip_hFacW
CADJ &     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE surfaceForcingU, surfaceForcingV =
CADJ &     comlev1, key = ikey_dynamics, kind = isbyte
# endif

cph the following needed to be moved here from do_oceanic_physics
cph to be visible down the road

CADJ STORE rhoInSitu      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE surfaceForcingS= comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE surfaceForcingT= comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE IVDConvCount   = comlev1, key = ikey_dynamics, kind = isbyte
# ifdef ALLOW_KPP
CADJ STORE adjustColdSST_diag = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
# endif
# ifdef ALLOW_PTRACERS
CADJ STORE surfaceForcingPTr  = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
# endif

# ifdef ALLOW_GMREDI
CADJ STORE Kwx            = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE Kwy            = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE Kwz            = comlev1, key = ikey_dynamics, kind = isbyte
#  ifdef GM_BOLUS_ADVEC
CADJ STORE GM_PsiX        = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE GM_PsiY        = comlev1, key = ikey_dynamics, kind = isbyte
#  endif
# endif

# ifdef ALLOW_KPP
CADJ STORE KPPghat        = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE KPPfrac        = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE KPPdiffKzS     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE KPPdiffKzT     = comlev1, key = ikey_dynamics, kind = isbyte
#  ifdef ALLOW_SALT_PLUME
CADJ STORE KPPplumefrac   = comlev1, key = ikey_dynamics, kind = isbyte
#  endif
# endif

# ifdef ALLOW_GGL90
CADJ STORE GGL90diffKr    = comlev1, key = ikey_dynamics, kind = isbyte
# endif

# ifdef ALLOW_SALT_PLUME
CADJ STORE SaltPlumeDepth = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE saltPlumeFlux  = comlev1, key = ikey_dynamics, kind = isbyte
# endif

# ifdef ALLOW_DOWN_SLOPE
CADJ STORE DWNSLP_deepK   = comlev1, key = ikey_dynamics
CADJ STORE DWNSLP_Transp  = comlev1, key = ikey_dynamics, kind = isbyte
# endif

# ifdef ALLOW_SHELFICE
CADJ STORE shelficeForcingT=comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE shelficeForcingS=comlev1, key = ikey_dynamics, kind = isbyte
# endif /* ALLOW_SHELFICE */

# if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
#  ifdef ALLOW_PTRACERS
CADJ STORE pTracer        = comlev1, key = ikey_dynamics, kind = isbyte
#  endif /* ALLOW_PTRACERS */
CADJ STORE theta,salt     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE etaH           = comlev1, key = ikey_dynamics, kind = isbyte
#  ifdef ALLOW_CD_CODE
CADJ STORE etaNm1         = comlev1, key = ikey_dynamics, kind = isbyte
#  endif
#  ifndef DISABLE_RSTAR_CODE
CADJ STORE rStarExpC      = comlev1, key = ikey_dynamics, kind = isbyte
#  endif
# endif
