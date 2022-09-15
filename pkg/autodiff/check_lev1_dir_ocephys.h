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
#ifdef EXACT_CONSERV
CADJ STORE PmEpR          = comlev1, key = ikey_dynamics, kind = isbyte
#endif
CADJ STORE qsw            = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE surfaceForcingU= comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE surfaceForcingV= comlev1, key = ikey_dynamics, kind = isbyte
#ifdef ATMOSPHERIC_LOADING
CADJ STORE phi0surf       = comlev1, key = ikey_dynamics, kind = isbyte
#endif
CADJ STORE sIceLoad       = comlev1, key = ikey_dynamics, kind = isbyte

#ifdef ALLOW_OBCS
CADJ STORE salt, theta = comlev1, key = ikey_dynamics, kind = isbyte
# ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNt        = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBNs        = comlev1, key = ikey_dynamics, kind = isbyte
#  ifdef ALLOW_OBCS_STEVENS
CADJ STORE OBNtStevens = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBNsStevens = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBNvStevens = comlev1, key = ikey_dynamics, kind = isbyte
#  endif /* ALLOW_OBCS_STEVENS */
# endif /* ALLOW_OBCS_NORTH */

# ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSt        = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBSs        = comlev1, key = ikey_dynamics, kind = isbyte
#  ifdef ALLOW_OBCS_STEVENS
CADJ STORE OBStStevens = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBSsStevens = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBSvStevens = comlev1, key = ikey_dynamics, kind = isbyte
#  endif /* ALLOW_OBCS_STEVENS */
# endif /* ALLOW_OBCS_SOUTH */

# ifdef ALLOW_OBCS_EAST
CADJ STORE OBEt        = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBEs        = comlev1, key = ikey_dynamics, kind = isbyte
#  ifdef ALLOW_OBCS_STEVENS
CADJ STORE OBEtStevens = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBEsStevens = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBEuStevens = comlev1, key = ikey_dynamics, kind = isbyte
#  endif /* ALLOW_OBCS_STEVENS */
# endif /* ALLOW_OBCS_EAST */

# ifdef ALLOW_OBCS_WEST
CADJ STORE OBWt        = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBWs        = comlev1, key = ikey_dynamics, kind = isbyte
#  ifdef ALLOW_OBCS_STEVENS
CADJ STORE OBWtStevens = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBWsStevens = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBWuStevens = comlev1, key = ikey_dynamics, kind = isbyte
#  endif /* ALLOW_OBCS_STEVENS */
# endif /* ALLOW_OBCS_WEST */

# ifdef ALLOW_PTRACERS
#  ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNptr = comlev1, key = ikey_dynamics, kind = isbyte
#  endif /* ALLOW_OBCS_NORTH */
#  ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSptr = comlev1, key = ikey_dynamics, kind = isbyte
#  endif /* ALLOW_OBCS_SOUTH */
#  ifdef ALLOW_OBCS_EAST
CADJ STORE OBEptr = comlev1, key = ikey_dynamics, kind = isbyte
#  endif /* ALLOW_OBCS_EAST */
#  ifdef ALLOW_OBCS_WEST
CADJ STORE OBWptr = comlev1, key = ikey_dynamics, kind = isbyte
#  endif /* ALLOW_OBCS_WEST */
# endif  /* ALLOW_PTRACERS */

# ifdef ALLOW_SEAICE
#  ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNh  = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBNa  = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBNsn = comlev1, key = ikey_dynamics, kind = isbyte
#  ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBNsl = comlev1, key = ikey_dynamics, kind = isbyte
#  endif
#  endif /* ALLOW_OBCS_NORTH */
#  ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSh  = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBSa  = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBSsn = comlev1, key = ikey_dynamics, kind = isbyte
#   ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBSsl = comlev1, key = ikey_dynamics, kind = isbyte
#   endif
#  endif /* ALLOW_OBCS_SOUTH */
#  ifdef ALLOW_OBCS_EAST
CADJ STORE OBEh  = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBEa  = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBEsn = comlev1, key = ikey_dynamics, kind = isbyte
#   ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBEsl = comlev1, key = ikey_dynamics, kind = isbyte
#   endif
#  endif /* ALLOW_OBCS_EAST */
#  ifdef ALLOW_OBCS_WEST
CADJ STORE OBWh  = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBWa  = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE OBWsn = comlev1, key = ikey_dynamics, kind = isbyte
#   ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE OBWsl = comlev1, key = ikey_dynamics, kind = isbyte
#   endif
#  endif /* ALLOW_OBCS_WEST */
# endif /* ALLOW_SEAICE */
#endif /* ALLOW_OBCS */

#ifdef ALLOW_DEPTH_CONTROL
CADJ STORE hFacC, hFacS, hFacW
CADJ &     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE recip_hFacC, recip_hFacS, recip_hFacW
CADJ &     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE surfaceForcingU, surfaceForcingV =
CADJ &     comlev1, key = ikey_dynamics, kind = isbyte
#endif

cph the following needed to be moved here from do_oceanic_physics
cph to be visible down the road

CADJ STORE rhoInSitu      = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE surfaceForcingS= comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE surfaceForcingT= comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE IVDConvCount   = comlev1, key = ikey_dynamics, kind = isbyte
#ifdef ALLOW_KPP
CADJ STORE adjustColdSST_diag = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#endif
#ifdef ALLOW_PTRACERS
CADJ STORE surfaceForcingPTr  = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#endif

#ifdef ALLOW_GMREDI
CADJ STORE Kwx            = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE Kwy            = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE Kwz            = comlev1, key = ikey_dynamics, kind = isbyte
# ifdef GM_BOLUS_ADVEC
CADJ STORE GM_PsiX        = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE GM_PsiY        = comlev1, key = ikey_dynamics, kind = isbyte
# endif
#endif

#ifdef ALLOW_KPP
CADJ STORE KPPghat        = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE KPPfrac        = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE KPPdiffKzS     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE KPPdiffKzT     = comlev1, key = ikey_dynamics, kind = isbyte
# ifdef ALLOW_SALT_PLUME
CADJ STORE KPPplumefrac   = comlev1, key = ikey_dynamics, kind = isbyte
# endif
#endif

#ifdef ALLOW_GGL90
CADJ STORE GGL90diffKr    = comlev1, key = ikey_dynamics, kind = isbyte
#endif

#ifdef ALLOW_SALT_PLUME
CADJ STORE SaltPlumeDepth = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE saltPlumeFlux  = comlev1, key = ikey_dynamics, kind = isbyte
#endif

#ifdef ALLOW_DOWN_SLOPE
CADJ STORE DWNSLP_deepK   = comlev1, key = ikey_dynamics
CADJ STORE DWNSLP_Transp  = comlev1, key = ikey_dynamics, kind = isbyte
#endif

#ifdef ALLOW_SHELFICE
CADJ STORE kTopC           =comlev1, key = ikey_dynamics
CADJ STORE shelficeForcingT=comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE shelficeForcingS=comlev1, key = ikey_dynamics, kind = isbyte
#endif /* ALLOW_SHELFICE */

#if (defined NONLIN_FRSURF) || (defined ALLOW_DEPTH_CONTROL)
CADJ STORE theta,salt     = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE etaH           = comlev1, key = ikey_dynamics, kind = isbyte
# ifdef ALLOW_CD_CODE
CADJ STORE etaNm1         = comlev1, key = ikey_dynamics, kind = isbyte
# endif
# ifdef ALLOW_PTRACERS
CADJ STORE pTracer        = comlev1, key = ikey_dynamics, kind = isbyte
# endif /* ALLOW_PTRACERS */
#endif /* NONLIN_FRSURF or ALLOW_DEPTH_CONTROL */
#ifdef NONLIN_FRSURF
# ifndef DISABLE_RSTAR_CODE
CADJ STORE rStarExpC      = comlev1, key = ikey_dynamics, kind = isbyte
# endif
#endif
