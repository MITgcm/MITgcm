CADJ STORE area     = tapelev3, key = ilev_3
CADJ STORE heff     = tapelev3, key = ilev_3
CADJ STORE hsnow    = tapelev3, key = ilev_3
CADJ STORE tice     = tapelev3, key = ilev_3
CADJ STORE runoff   = tapelev3, key = ilev_3
# ifdef SEAICE_MULTICATEGORY
CADJ STORE tices    = tapelev3, key = ilev_3
# endif
# ifdef SEAICE_ALLOW_DYNAMICS
CADJ STORE uice     = tapelev3, key = ilev_3
CADJ STORE vice     = tapelev3, key = ilev_3
CADJ STORE zeta     = tapelev3, key = ilev_3
CADJ STORE eta      = tapelev3, key = ilev_3
#  ifdef SEAICE_CGRID
CADJ STORE dwatn = tapelev3, key = ilev_3
CADJ STORE seaicemasku,seaicemaskv = tapelev3, key = ilev_3
#  endif
#  ifdef SEAICE_ALLOW_EVP
CADJ STORE seaice_sigma1  = tapelev3, key = ilev_3
CADJ STORE seaice_sigma2  = tapelev3, key = ilev_3
CADJ STORE seaice_sigma12 = tapelev3, key = ilev_3
#  endif
# endif
#ifdef ALLOW_COST_ICE
CADJ STORE objf_ice = tapelev3, key = ilev_3
#endif
