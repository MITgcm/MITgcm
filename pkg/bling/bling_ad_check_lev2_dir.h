#ifdef ALLOW_BLING

CADJ STORE pH                = tapelev2, key = ilev_2
cCADJ STORE fice              = tapelev2, key = ilev_2
cCADJ STORE atmosP            = tapelev2, key = ilev_2
cCADJ STORE wind              = tapelev2, key = ilev_2
cCADJ STORE silica            = tapelev2, key = ilev_2
cCADJ STORE ak1               = tapelev2, key = ilev_2
cCADJ STORE ak2               = tapelev2, key = ilev_2
cCADJ STORE akf               = tapelev2, key = ilev_2
cCADJ STORE ft                = tapelev2, key = ilev_2

CADJ STORE irr_mem           = tapelev2, key = ilev_2
#ifdef ALLOW_SELF_SHADING
CADJ STORE chl               = tapelev2, key = ilev_2
#endif
cCADJ STORE poc               = tapelev2, key = ilev_2
CADJ STORE phyto_sm          = tapelev2, key = ilev_2
CADJ STORE phyto_lg          = tapelev2, key = ilev_2
# ifndef USE_BLING_V1
CADJ STORE phyto_diaz        = tapelev2, key = ilev_2
# endif

CADJ STORE dicwind0          = tapelev2, key = ilev_2
CADJ STORE dicwind1          = tapelev2, key = ilev_2
CADJ STORE atmosp0           = tapelev2, key = ilev_2
CADJ STORE atmosp1           = tapelev2, key = ilev_2
CADJ STORE silica0           = tapelev2, key = ilev_2
CADJ STORE silica1           = tapelev2, key = ilev_2
CADJ STORE ice0              = tapelev2, key = ilev_2
CADJ STORE ice1              = tapelev2, key = ilev_2
CADJ STORE feinput0          = tapelev2, key = ilev_2
CADJ STORE feinput1          = tapelev2, key = ilev_2

#ifdef ALLOW_EXF
CADJ STORE apressure         = tapelev2, key = ilev_2
CADJ STORE wspeed            = tapelev2, key = ilev_2
CADJ STORE apco20            = tapelev2, key = ilev_2
CADJ STORE apco21            = tapelev2, key = ilev_2
# endif

#endif /* ALLOW_BLING */
