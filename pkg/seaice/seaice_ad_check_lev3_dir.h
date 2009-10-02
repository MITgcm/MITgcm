C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_ad_check_lev3_dir.h,v 1.14 2009/10/02 13:55:53 heimbach Exp $
C $Name:  $

CADJ STORE StoreSEAICE    = tapelev3, key = ilev_3

#ifdef SEAICE_MULTICATEGORY
CADJ STORE tices      =     tapelev3, key = ilev_3
#endif

#ifdef ALLOW_COST_ICE
CADJ STORE objf_ice = tapelev3, key = ilev_3
#endif
#ifdef ALLOW_SEAICE_COST_EXPORT
CADJ STORE uHeffExportCell = tapelev3, key = ilev_3
CADJ STORE vHeffExportCell = tapelev3, key = ilev_3
#endif
