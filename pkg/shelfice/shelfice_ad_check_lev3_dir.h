#ifdef ALLOW_SHELFICE
CADJ STORE shelficeMass     = tapelev3, key = ilev_3
CADJ STORE kTopC            = tapelev3, key = ilev_3
CADJ STORE shelficeForcingT = tapelev3, key = ilev_3
CADJ STORE shelficeForcingS = tapelev3, key = ilev_3
CADJ STORE cMeanSHIforT     = tapelev3, key = ilev_3
CADJ STORE cMeanSHIforS     = tapelev3, key = ilev_3
# ifdef ALLOW_SHIFWFLX_CONTROL
CADJ STORE xx_shifwflx0     = tapelev3, key = ilev_3
CADJ STORE xx_shifwflx1     = tapelev3, key = ilev_3
# endif
#endif /* ALLOW_SHELFICE */

