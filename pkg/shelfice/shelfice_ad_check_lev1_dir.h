#ifdef ALLOW_SHELFICE
CADJ STORE shelficeMass     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE kTopC            = comlev1, key=ikey_dynamics
CADJ STORE shelficeForcingT = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE shelficeForcingS = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE cMeanSHIforT     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE cMeanSHIforS     = comlev1, key=ikey_dynamics, kind=isbyte
# ifdef ALLOW_STEEP_ICECAVITY
CADJ STORE shelficeFreshWaterFlux
CADJ &                      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE shelficeLoadAnomaly
CADJ &                      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE shiTransCoeffS   = comlev1, key=ikey_dynamics, kind=isbyte
# endif
#endif /* ALLOW_SHELFICE */
