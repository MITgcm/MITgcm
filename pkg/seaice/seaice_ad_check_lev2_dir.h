C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_ad_check_lev2_dir.h,v 1.14 2010/12/17 04:02:25 gforget Exp $
C $Name:  $

CADJ STORE StoreSEAICE    = tapelev2, key = ilev_2

#ifdef SEAICE_MULTICATEGORY
CADJ STORE tices      =     tapelev2, key = ilev_2
#endif

#ifdef NONLIN_FRSURF
CADJ STORE area  = tapelev2, key = ilev_2
CADJ STORE heff  = tapelev2, key = ilev_2
CADJ STORE uice  = tapelev2, key = ilev_2
CADJ STORE vice  = tapelev2, key = ilev_2
#endif

#ifdef ALLOW_COST_ICE
CADJ STORE objf_ice = tapelev2, key = ilev_2
#endif
#ifdef ALLOW_SEAICE_COST_EXPORT
CADJ STORE uHeffExportCell = tapelev2, key = ilev_2
CADJ STORE vHeffExportCell = tapelev2, key = ilev_2
#endif
