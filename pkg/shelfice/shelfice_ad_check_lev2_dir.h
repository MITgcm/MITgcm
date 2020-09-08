#ifdef ALLOW_SHELFICE
CADJ STORE shelficeMass     = tapelev2, key = ilev_2
CADJ STORE kTopC            = tapelev2, key = ilev_2
CADJ STORE shelficeForcingT = tapelev2, key = ilev_2
CADJ STORE shelficeForcingS = tapelev2, key = ilev_2
CADJ STORE cMeanSHIforT     = tapelev2, key = ilev_2
CADJ STORE cMeanSHIforS     = tapelev2, key = ilev_2
# ifdef ALLOW_SHIFWFLX_CONTROL
CADJ STORE xx_shifwflx0     = tapelev2, key = ilev_2
CADJ STORE xx_shifwflx1     = tapelev2, key = ilev_2
# endif
#endif /* ALLOW_SHELFICE */
