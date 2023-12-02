#ifdef ALLOW_SHELFICE
CADJ STORE phi0surf         = tapelev3, key = ilev_3
CADJ STORE shelficeMass     = tapelev3, key = ilev_3
CADJ STORE kTopC            = tapelvi3, key = ilev_3
CADJ STORE shelficeForcingT = tapelev3, key = ilev_3
CADJ STORE shelficeForcingS = tapelev3, key = ilev_3
CADJ STORE cMeanSHIforT     = tapelev3, key = ilev_3
CADJ STORE cMeanSHIforS     = tapelev3, key = ilev_3
CADJ STORE shelficeFreshWaterFlux = tapelev3, key = ilev_3
CADJ STORE shelficeLoadAnomaly    = tapelev3, key = ilev_3
#endif /* ALLOW_SHELFICE */
#ifdef ALLOW_STEEP_ICECAVITY
#ifdef ALLOW_SHITRANSCOEFF_3D
CADJ STORE shiTransCoeffS3d = tapelev3, key = ilev_3
#else
CADJ STORE shiTransCoeffS   = tapelev3, key = ilev_3
#endif
#endif /* ALLOW_STEEP_ICECAVITY */
