#ifdef ALLOW_BLING

CADJ STORE pH                = tapelev2, key = ilev_2

CADJ STORE irr_mem           = tapelev2, key = ilev_2
# ifdef PHYTO_SELF_SHADING
CADJ STORE chl               = tapelev2, key = ilev_2
# endif
CADJ STORE phyto_sm          = tapelev2, key = ilev_2
CADJ STORE phyto_lg          = tapelev2, key = ilev_2
# ifndef USE_BLING_V1
CADJ STORE phyto_diaz        = tapelev2, key = ilev_2
# endif

#ifdef ALLOW_EXF
cCADJ STORE apressure         = tapelev2, key = ilev_2
cCADJ STORE wspeed            = tapelev2, key = ilev_2
# endif

#endif /* ALLOW_BLING */
