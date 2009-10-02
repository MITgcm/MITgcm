C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_ad_check_lev4_dir.h,v 1.11 2009/10/02 13:55:53 heimbach Exp $
C $Name:  $

CADJ STORE StoreSEAICE    = tapelev4, key = ilev_4

#ifdef SEAICE_MULTICATEGORY
CADJ STORE tices      =     tapelev4, key = ilev_4
#endif

#ifdef ALLOW_COST_ICE
CADJ STORE objf_ice = tapelev4, key = ilev_4
#endif
#ifdef ALLOW_SEAICE_COST_EXPORT
CADJ STORE uHeffExportCell = tapelev4, key = ilev_4
CADJ STORE vHeffExportCell = tapelev4, key = ilev_4
#endif
