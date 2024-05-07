#ifdef ALLOW_SHELFICE
# ifdef ALLOW_SHELFICE_REMESHING
CADJ STORE kTopC            = tapelvi4, key = ilev_4
# endif
CADJ STORE phi0surf         = tapelev4, key = ilev_4
CADJ STORE shelficeMass     = tapelev4, key = ilev_4
CADJ STORE shelficeForcingT = tapelev4, key = ilev_4
CADJ STORE shelficeForcingS = tapelev4, key = ilev_4
CADJ STORE cMeanSHIforT     = tapelev4, key = ilev_4
CADJ STORE cMeanSHIforS     = tapelev4, key = ilev_4
CADJ STORE shelficeFreshWaterFlux = tapelev4, key = ilev_4
CADJ STORE shelficeLoadAnomaly    = tapelev4, key = ilev_4
#endif /* ALLOW_SHELFICE */
