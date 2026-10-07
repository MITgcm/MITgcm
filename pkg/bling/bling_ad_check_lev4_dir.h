#ifdef ALLOW_BLING

CADJ STORE pH                = tapelev4, key = ilev_4

CADJ STORE irr_mem           = tapelev4, key = ilev_4
# ifdef PHYTO_SELF_SHADING
CADJ STORE chl               = tapelev4, key = ilev_4
# endif
CADJ STORE phyto_sm          = tapelev4, key = ilev_4
CADJ STORE phyto_lg          = tapelev4, key = ilev_4
# ifndef USE_BLING_V1
CADJ STORE phyto_diaz        = tapelev4, key = ilev_4
# endif

#ifdef ALLOW_EXF
CADJ STORE apressure         = tapelev4, key = ilev_4
CADJ STORE wspeed            = tapelev4, key = ilev_4
# endif

#endif /* ALLOW_BLING */
