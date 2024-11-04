#ifdef ALLOW_BLING

CADJ STORE pH                = tapelev3, key = ilev_3
cCADJ STORE fice              = tapelev3, key = ilev_3
cCADJ STORE atmosP            = tapelev3, key = ilev_3
cCADJ STORE wind              = tapelev3, key = ilev_3
cCADJ STORE silica            = tapelev3, key = ilev_3
cCADJ STORE ak1               = tapelev3, key = ilev_3
cCADJ STORE ak2               = tapelev3, key = ilev_3
cCADJ STORE akf               = tapelev3, key = ilev_3
cCADJ STORE ft                = tapelev3, key = ilev_3

CADJ STORE irr_mem           = tapelev3, key = ilev_3
# ifdef PHYTO_SELF_SHADING
CADJ STORE chl               = tapelev3, key = ilev_3
# endif
cCADJ STORE poc               = tapelev3, key = ilev_3
CADJ STORE phyto_sm          = tapelev3, key = ilev_3
CADJ STORE phyto_lg          = tapelev3, key = ilev_3
# ifndef USE_BLING_V1
CADJ STORE phyto_diaz        = tapelev3, key = ilev_3
# endif

CADJ STORE dicwind0          = tapelev3, key = ilev_3
CADJ STORE dicwind1          = tapelev3, key = ilev_3
CADJ STORE atmosp0           = tapelev3, key = ilev_3
CADJ STORE atmosp1           = tapelev3, key = ilev_3
CADJ STORE silica0           = tapelev3, key = ilev_3
CADJ STORE silica1           = tapelev3, key = ilev_3
CADJ STORE ice0              = tapelev3, key = ilev_3
CADJ STORE ice1              = tapelev3, key = ilev_3
CADJ STORE feinput0          = tapelev3, key = ilev_3
CADJ STORE feinput1          = tapelev3, key = ilev_3

#ifdef ALLOW_EXF
CADJ STORE apressure         = tapelev3, key = ilev_3
CADJ STORE wspeed            = tapelev3, key = ilev_3
CADJ STORE apco20            = tapelev3, key = ilev_3
CADJ STORE apco21            = tapelev3, key = ilev_3
# endif

#endif /* ALLOW_BLING */
