C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_ad_check_lev4_dir.h,v 1.10 2007/10/09 00:10:13 jmc Exp $
C $Name:  $

CADJ STORE StoreSEAICE    = tapelev4, key = ilev_4

#ifdef ALLOW_COST_ICE
CADJ STORE objf_ice = tapelev4, key = ilev_4
#endif
#ifdef ALLOW_SEAICE_COST_EXPORT
CADJ STORE uHeffExportCell = tapelev4, key = ilev_4
CADJ STORE vHeffExportCell = tapelev4, key = ilev_4
#endif
