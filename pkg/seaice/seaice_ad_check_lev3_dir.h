CADJ STORE area     = tapelev3, key = ilev_3
CADJ STORE heff     = tapelev3, key = ilev_3
CADJ STORE hsnow    = tapelev3, key = ilev_3
CADJ STORE tice     = tapelev3, key = ilev_3
CADJ STORE runoff   = tapelev3, key = ilev_3
# ifdef SEAICE_MULTILEVEL
CADJ STORE tices    = tapelev3, key = ilev_3
# endif
# ifdef SEAICE_ALLOW_DYNAMICS
CADJ STORE uice     = tapelev3, key = ilev_3
CADJ STORE vice     = tapelev3, key = ilev_3
#  ifdef SEAICE_CGRID
CADJ STORE dwatn = tapelev3, key = ilev_3
CADJ STORE seaicemasku,seaicemaskv = tapelev3, key = ilev_3
CADJ STORE uicec,vicec = tapelev3, key = ilev_3
#  endif
# endif
