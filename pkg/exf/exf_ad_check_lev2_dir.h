#ifdef ALLOW_EXF

CADJ STORE StoreEXF1    = tapelev2, key = ilev_2
CADJ STORE StoreEXF2    = tapelev2, key = ilev_2
CADJ STORE StoreCTRLS1  = tapelev2, key = ilev_2

# ifdef EXF_SEAICE_FRACTION
CADJ STORE areamask0    = tapelev2, key = ilev_2
CADJ STORE areamask1    = tapelev2, key = ilev_2
# endif

# ifdef ECCO_CTRL_DEPRECATED
# ifdef ALLOW_HFLUX_CONTROL
CADJ STORE xx_hflux0     = tapelev2, key = ilev_2
CADJ STORE xx_hflux1     = tapelev2, key = ilev_2
# endif
# ifdef ALLOW_SFLUX_CONTROL
CADJ STORE xx_sflux0     = tapelev2, key = ilev_2
CADJ STORE xx_sflux1     = tapelev2, key = ilev_2
# endif
# ifdef ALLOW_USTRESS_CONTROL
CADJ STORE xx_tauu0      = tapelev2, key = ilev_2
CADJ STORE xx_tauu1      = tapelev2, key = ilev_2
# endif
# ifdef ALLOW_VSTRESS_CONTROL
CADJ STORE xx_tauv0      = tapelev2, key = ilev_2
CADJ STORE xx_tauv1      = tapelev2, key = ilev_2
# endif
# ifdef ALLOW_SST_CONTROL
CADJ STORE xx_sst0      = tapelev2, key = ilev_2
CADJ STORE xx_sst1      = tapelev2, key = ilev_2
# endif
# ifdef ALLOW_SSS_CONTROL
CADJ STORE xx_sss0      = tapelev2, key = ilev_2
CADJ STORE xx_sss1      = tapelev2, key = ilev_2
# endif
# endif /* ECCO_CTRL_DEPRECATED */

#endif /* ALLOW_EXF */
