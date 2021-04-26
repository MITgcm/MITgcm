#ifdef ALLOW_SHELFICE
CADJ STORE shelficeMass     = tapelev4, key = ilev_4
CADJ STORE kTopC            = tapelev4, key = ilev_4
CADJ STORE shelficeForcingT = tapelev4, key = ilev_4
CADJ STORE shelficeForcingS = tapelev4, key = ilev_4
CADJ STORE cMeanSHIforT     = tapelev4, key = ilev_4
CADJ STORE cMeanSHIforS     = tapelev4, key = ilev_4
# ifdef ALLOW_SHIFWFLX_CONTROL
CADJ STORE xx_shifwflx0     = tapelev4, key = ilev_4
CADJ STORE xx_shifwflx1     = tapelev4, key = ilev_4
# endif
#endif /* ALLOW_SHELFICE */
