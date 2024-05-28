#ifdef ALLOW_BLING

CADJ STORE pH                = tapelev4, key = ilev_4
cCADJ STORE fice              = tapelev4, key = ilev_4
cCADJ STORE atmosP            = tapelev4, key = ilev_4
cCADJ STORE wind              = tapelev4, key = ilev_4
cCADJ STORE silica            = tapelev4, key = ilev_4
cCADJ STORE ak1               = tapelev4, key = ilev_4
cCADJ STORE ak2               = tapelev4, key = ilev_4
cCADJ STORE akf               = tapelev4, key = ilev_4
cCADJ STORE ft                = tapelev4, key = ilev_4

CADJ STORE irr_mem           = tapelev4, key = ilev_4
CADJ STORE chl               = tapelev4, key = ilev_4
CADJ STORE phyto_sm          = tapelev4, key = ilev_4
CADJ STORE phyto_lg          = tapelev4, key = ilev_4
# ifndef USE_BLING_V1
CADJ STORE phyto_diaz        = tapelev4, key = ilev_4
# endif

CADJ STORE dicwind0          = tapelev4, key = ilev_4
CADJ STORE dicwind1          = tapelev4, key = ilev_4
CADJ STORE atmosp0           = tapelev4, key = ilev_4
CADJ STORE atmosp1           = tapelev4, key = ilev_4
CADJ STORE silica0           = tapelev4, key = ilev_4
CADJ STORE silica1           = tapelev4, key = ilev_4
CADJ STORE ice0              = tapelev4, key = ilev_4
CADJ STORE ice1              = tapelev4, key = ilev_4
CADJ STORE feinput0          = tapelev4, key = ilev_4
CADJ STORE feinput1          = tapelev4, key = ilev_4

#ifdef ALLOW_EXF
CADJ STORE apressure         = tapelev4, key = ilev_4
CADJ STORE wspeed            = tapelev4, key = ilev_4
CADJ STORE apco20            = tapelev4, key = ilev_4
CADJ STORE apco21            = tapelev4, key = ilev_4
# endif

#endif /* ALLOW_BLING */
