#ifdef AUTODIFF_USE_STORE_RESTORE
CADJ STORE StoreSEAICE       = tapelev2, key = ilev_2
#else
CADJ STORE AREA              = tapelev2, key = ilev_2
CADJ STORE HEFF              = tapelev2, key = ilev_2
CADJ STORE HSNOW             = tapelev2, key = ilev_2
CADJ STORE RUNOFF            = tapelev2, key = ilev_2
CADJ STORE UICE              = tapelev2, key = ilev_2
CADJ STORE VICE              = tapelev2, key = ilev_2
CADJ STORE ZETA              = tapelev2, key = ilev_2
CADJ STORE ETA               = tapelev2, key = ilev_2
CADJ STORE TICES             = tapelev2, key = ilev_2
# ifdef SEAICE_CGRID
CADJ STORE dwatn             = tapelev2, key = ilev_2
#  ifdef SEAICE_ALLOW_BOTTOMDRAG
CADJ STORE cbotc             = tapelev2, key = ilev_2
#  endif /* SEAICE_ALLOW_BOTTOMDRAG */
CADJ STORE stressDivergenceX = tapelev2, key = ilev_2
CADJ STORE stressDivergenceY = tapelev2, key = ilev_2
# ifdef SEAICE_ALLOW_EVP
CADJ STORE seaice_sigma1     = tapelev2, key = ilev_2
CADJ STORE seaice_sigma2     = tapelev2, key = ilev_2
CADJ STORE seaice_sigma12    = tapelev2, key = ilev_2
# endif /* SEAICE_ALLOW_EVP */
# endif /* SEAICE_CGRID */
# ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE HSALT             = tapelev2, key = ilev_2
# endif
#endif /* AUTODIFF_USE_STORE_RESTORE */

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
