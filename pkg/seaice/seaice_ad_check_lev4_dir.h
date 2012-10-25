C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_ad_check_lev4_dir.h,v 1.18 2012/10/25 19:43:44 heimbach Exp $
C $Name:  $

CADJ STORE StoreSEAICE    = tapelev4, key = ilev_4
CADJ STORE tices      =     tapelev4, key = ilev_4

#ifdef AUTODIFF_SOMETIMES_NEEDED
CADJ STORE area  = tapelev4, key = ilev_4
CADJ STORE heff  = tapelev4, key = ilev_4
CADJ STORE uice  = tapelev4, key = ilev_4
CADJ STORE vice  = tapelev4, key = ilev_4
#endif

#ifdef ALLOW_COST_ICE
CADJ STORE objf_ice = tapelev4, key = ilev_4
#endif
#ifdef ALLOW_SEAICE_COST_EXPORT
CADJ STORE uHeffExportCell = tapelev4, key = ilev_4
CADJ STORE vHeffExportCell = tapelev4, key = ilev_4
CADJ STORE icevolMeanCell = tapelev4, key = ilev_4
#endif
#if (defined (ALLOW_MEAN_SFLUX_COST_CONTRIBUTION) || defined (ALLOW_SSH_GLOBMEAN_COST_CONTRIBUTION))
CADJ STORE AREAforAtmFW = tapelev4, key = ilev_4
#endif

#ifdef ALLOW_OBCS
# ifdef ALLOW_OBCS_EAST
CADJ STORE obeuice,obeuice0,obeuice1 = tapelev4, key = ilev_4
CADJ STORE obevice,obevice0,obevice1 = tapelev4, key = ilev_4
# endif
# ifdef ALLOW_OBCS_NORTH
CADJ STORE obnuice,obnuice0,obnuice1 = tapelev4, key = ilev_4
CADJ STORE obnvice,obnvice0,obnvice1 = tapelev4, key = ilev_4
# endif
# ifdef ALLOW_OBCS_SOUTH
CADJ STORE obsuice,obsuice0,obsuice1 = tapelev4, key = ilev_4
CADJ STORE obsvice,obsvice0,obsvice1 = tapelev4, key = ilev_4
# endif
# ifdef ALLOW_OBCS_WEST
CADJ STORE obwuice,obwuice0,obwuice1 = tapelev4, key = ilev_4
CADJ STORE obwvice,obwvice0,obwvice1 = tapelev4, key = ilev_4
# endif
#endif

#ifdef ALLOW_SITRACER
CADJ STORE sitracer = tapelev4, key = ilev_4
CADJ STORE sitrarea = tapelev4, key = ilev_4
CADJ STORE sitrheff = tapelev4, key = ilev_4
#endif

