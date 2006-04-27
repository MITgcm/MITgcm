CADJ STORE area     = tapelev2, key = ilev_2
CADJ STORE heff     = tapelev2, key = ilev_2
CADJ STORE hsnow    = tapelev2, key = ilev_2
CADJ STORE tice     = tapelev2, key = ilev_2
CADJ STORE runoff   = tapelev2, key = ilev_2
# ifdef SEAICE_MULTILEVEL
CADJ STORE tices    = tapelev2, key = ilev_2
# endif
# ifdef SEAICE_ALLOW_DYNAMICS
CADJ STORE uice     = tapelev2, key = ilev_2
CADJ STORE vice     = tapelev2, key = ilev_2
#  ifdef SEAICE_CGRID
CADJ STORE dwatn = tapelev2, key = ilev_2
CADJ STORE seaicemasku,seaicemaskv = tapelev2, key = ilev_2
CADJ STORE uicec,vicec = tapelev2, key = ilev_2
#  endif
# endif
