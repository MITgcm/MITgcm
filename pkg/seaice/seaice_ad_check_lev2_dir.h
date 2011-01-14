C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_ad_check_lev2_dir.h,v 1.15 2011/01/14 01:34:58 gforget Exp $
C $Name:  $

CADJ STORE StoreSEAICE    = tapelev2, key = ilev_2

#ifdef SEAICE_MULTICATEGORY
CADJ STORE tices      =     tapelev2, key = ilev_2
#endif

#ifdef AUTODIFF_SOMETIMES_NEEDED
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
