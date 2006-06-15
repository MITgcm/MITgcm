CADJ STORE area     = tapelev4, key = ilev_4
CADJ STORE heff     = tapelev4, key = ilev_4
CADJ STORE hsnow    = tapelev4, key = ilev_4
CADJ STORE tice     = tapelev4, key = ilev_4
CADJ STORE runoff   = tapelev4, key = ilev_4
# ifdef SEAICE_MULTILEVEL
CADJ STORE tices    = tapelev4, key = ilev_4
# endif
# ifdef SEAICE_ALLOW_DYNAMICS
CADJ STORE uice     = tapelev4, key = ilev_4
CADJ STORE vice     = tapelev4, key = ilev_4
#  ifdef SEAICE_CGRID
CADJ STORE dwatn    = tapelev4, key = ilev_4
CADJ STORE seaicemasku,seaicemaskv = tapelev4, key = ilev_4
#  endif
# endif
#ifdef ALLOW_COST_ICE
CADJ STORE objf_ice = tapelev4, key = ilev_4
#endif
