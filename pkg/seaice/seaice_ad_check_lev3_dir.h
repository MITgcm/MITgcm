#ifdef AUTODIFF_USE_STORE_RESTORE
CADJ STORE StoreSEAICE       = tapelev3, key = ilev_3
#else
CADJ STORE AREA              = tapelev3, key = ilev_3
CADJ STORE HEFF              = tapelev3, key = ilev_3
CADJ STORE HSNOW             = tapelev3, key = ilev_3
CADJ STORE RUNOFF            = tapelev3, key = ilev_3
CADJ STORE UICE              = tapelev3, key = ilev_3
CADJ STORE VICE              = tapelev3, key = ilev_3
CADJ STORE ZETA              = tapelev3, key = ilev_3
CADJ STORE ETA               = tapelev3, key = ilev_3
CADJ STORE TICES             = tapelev3, key = ilev_3
# ifdef SEAICE_CGRID
CADJ STORE dwatn             = tapelev3, key = ilev_3
#  ifdef SEAICE_ALLOW_BOTTOMDRAG
CADJ STORE cbotc             = tapelev3, key = ilev_3
#  endif /* SEAICE_ALLOW_BOTTOMDRAG */
CADJ STORE stressDivergenceX = tapelev3, key = ilev_3
CADJ STORE stressDivergenceY = tapelev3, key = ilev_3
# ifdef SEAICE_ALLOW_EVP
CADJ STORE seaice_sigma1     = tapelev3, key = ilev_3
CADJ STORE seaice_sigma2     = tapelev3, key = ilev_3
CADJ STORE seaice_sigma12    = tapelev3, key = ilev_3
# endif /* SEAICE_ALLOW_EVP */
# endif /* SEAICE_CGRID */
# ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE HSALT             = tapelev3, key = ilev_3
# endif
#endif /* AUTODIFF_USE_STORE_RESTORE */

#ifdef AUTODIFF_SOMETIMES_NEEDED
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
CADJ STORE icevolMeanCell = tapelev3, key = ilev_3
#endif
#if (defined (ALLOW_MEAN_SFLUX_COST_CONTRIBUTION) || defined (ALLOW_SSH_GLOBMEAN_COST_CONTRIBUTION))
CADJ STORE AREAforAtmFW = tapelev3, key = ilev_3
#endif

#ifdef ALLOW_OBCS
# ifdef ALLOW_OBCS_EAST
CADJ STORE obeuice,obeuice0,obeuice1 = tapelev3, key = ilev_3
CADJ STORE obevice,obevice0,obevice1 = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_OBCS_NORTH
CADJ STORE obnuice,obnuice0,obnuice1 = tapelev3, key = ilev_3
CADJ STORE obnvice,obnvice0,obnvice1 = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_OBCS_SOUTH
CADJ STORE obsuice,obsuice0,obsuice1 = tapelev3, key = ilev_3
CADJ STORE obsvice,obsvice0,obsvice1 = tapelev3, key = ilev_3
# endif
# ifdef ALLOW_OBCS_WEST
CADJ STORE obwuice,obwuice0,obwuice1 = tapelev3, key = ilev_3
CADJ STORE obwvice,obwvice0,obwvice1 = tapelev3, key = ilev_3
# endif
#endif

#ifdef ALLOW_SITRACER
CADJ STORE sitracer = tapelev3, key = ilev_3
CADJ STORE sitrarea = tapelev3, key = ilev_3
CADJ STORE sitrheff = tapelev3, key = ilev_3
#endif
