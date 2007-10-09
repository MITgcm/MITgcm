C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_ad_check_lev2_dir.h,v 1.12 2007/10/09 00:10:13 jmc Exp $
C $Name:  $

CADJ STORE StoreSEAICE    = tapelev2, key = ilev_2

#ifdef ALLOW_COST_ICE
CADJ STORE objf_ice = tapelev2, key = ilev_2
#endif
#ifdef ALLOW_SEAICE_COST_EXPORT
CADJ STORE uHeffExportCell = tapelev2, key = ilev_2
CADJ STORE vHeffExportCell = tapelev2, key = ilev_2
#endif
