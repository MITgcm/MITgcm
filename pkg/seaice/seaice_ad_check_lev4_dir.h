#ifdef AUTODIFF_USE_STORE_RESTORE
CADJ STORE StoreSEAICE       = tapelev4, key = ilev_4
#else
CADJ STORE AREA              = tapelev4, key = ilev_4
CADJ STORE HEFF              = tapelev4, key = ilev_4
CADJ STORE HSNOW             = tapelev4, key = ilev_4
CADJ STORE RUNOFF            = tapelev4, key = ilev_4
CADJ STORE UICE              = tapelev4, key = ilev_4
CADJ STORE VICE              = tapelev4, key = ilev_4
CADJ STORE ZETA              = tapelev4, key = ilev_4
CADJ STORE ETA               = tapelev4, key = ilev_4
CADJ STORE TICES             = tapelev4, key = ilev_4
# ifdef SEAICE_CGRID
CADJ STORE dwatn             = tapelev4, key = ilev_4
#  ifdef SEAICE_ALLOW_BOTTOMDRAG
CADJ STORE cbotc             = tapelev4, key = ilev_4
#  endif /* SEAICE_ALLOW_BOTTOMDRAG */
CADJ STORE stressDivergenceX = tapelev4, key = ilev_4
CADJ STORE stressDivergenceY = tapelev4, key = ilev_4
# ifdef SEAICE_ALLOW_EVP
CADJ STORE seaice_sigma1     = tapelev4, key = ilev_4
CADJ STORE seaice_sigma2     = tapelev4, key = ilev_4
CADJ STORE seaice_sigma12    = tapelev4, key = ilev_4
# endif /* SEAICE_ALLOW_EVP */
# endif /* SEAICE_CGRID */
# ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE HSALT             = tapelev4, key = ilev_4
# endif
#endif /* AUTODIFF_USE_STORE_RESTORE */

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
