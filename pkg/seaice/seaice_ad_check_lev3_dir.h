C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_ad_check_lev3_dir.h,v 1.15 2010/12/17 04:02:25 gforget Exp $
C $Name:  $

CADJ STORE StoreSEAICE    = tapelev3, key = ilev_3

#ifdef SEAICE_MULTICATEGORY
CADJ STORE tices      =     tapelev3, key = ilev_3
#endif

#ifdef NONLIN_FRSURF
CADJ STORE area  = tapelev3, key = ilev_3
CADJ STORE heff  = tapelev3, key = ilev_3
CADJ STORE uice  = tapelev3, key = ilev_3
CADJ STORE vice  = tapelev3, key = ilev_3
#endif

#ifdef ALLOW_COST_ICE
CADJ STORE objf_ice = tapelev3, key = ilev_3
#endif
#ifdef ALLOW_SEAICE_COST_EXPORT
CADJ STORE uHeffExportCell = tapelev3, key = ilev_3
CADJ STORE vHeffExportCell = tapelev3, key = ilev_3
#endif
