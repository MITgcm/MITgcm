C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_ad_check_lev2_dir.h,v 1.20 2012/10/25 19:43:44 heimbach Exp $
C $Name:  $

CADJ STORE StoreSEAICE    = tapelev2, key = ilev_2
CADJ STORE tices      =     tapelev2, key = ilev_2

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
CADJ STORE icevolMeanCell = tapelev2, key = ilev_2
#endif
#if (defined (ALLOW_MEAN_SFLUX_COST_CONTRIBUTION) || defined (ALLOW_SSH_GLOBMEAN_COST_CONTRIBUTION))
CADJ STORE AREAforAtmFW = tapelev2, key = ilev_2
#endif

#ifdef ALLOW_OBCS
# ifdef ALLOW_OBCS_EAST
CADJ STORE obeuice,obeuice0,obeuice1 = tapelev2, key = ilev_2
CADJ STORE obevice,obevice0,obevice1 = tapelev2, key = ilev_2
# endif
# ifdef ALLOW_OBCS_NORTH
CADJ STORE obnuice,obnuice0,obnuice1 = tapelev2, key = ilev_2
CADJ STORE obnvice,obnvice0,obnvice1 = tapelev2, key = ilev_2
# endif
# ifdef ALLOW_OBCS_SOUTH
CADJ STORE obsuice,obsuice0,obsuice1 = tapelev2, key = ilev_2
CADJ STORE obsvice,obsvice0,obsvice1 = tapelev2, key = ilev_2
# endif
# ifdef ALLOW_OBCS_WEST
CADJ STORE obwuice,obwuice0,obwuice1 = tapelev2, key = ilev_2
CADJ STORE obwvice,obwvice0,obwvice1 = tapelev2, key = ilev_2
# endif
#endif

#ifdef ALLOW_SITRACER
CADJ STORE sitracer = tapelev2, key = ilev_2
CADJ STORE sitrarea = tapelev2, key = ilev_2
CADJ STORE sitrheff = tapelev2, key = ilev_2
#endif

