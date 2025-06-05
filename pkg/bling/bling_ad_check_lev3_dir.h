#ifdef ALLOW_BLING

CADJ STORE pH                = tapelev3, key = ilev_3

CADJ STORE irr_mem           = tapelev3, key = ilev_3
# ifdef PHYTO_SELF_SHADING
CADJ STORE chl               = tapelev3, key = ilev_3
# endif
CADJ STORE phyto_sm          = tapelev3, key = ilev_3
CADJ STORE phyto_lg          = tapelev3, key = ilev_3
# ifndef USE_BLING_V1
CADJ STORE phyto_diaz        = tapelev3, key = ilev_3
# endif

#ifdef ALLOW_EXF
CADJ STORE apressure         = tapelev3, key = ilev_3
CADJ STORE wspeed            = tapelev3, key = ilev_3
# endif

#endif /* ALLOW_BLING */
