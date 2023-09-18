#ifdef ALLOW_STEEP_ICECAVITY
CADJ STORE sticMass         = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE kTopC            = comlev1, key=ikey_dynamics
CADJ STORE shelficeForcingT = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE shelficeForcingS = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE cMeanSHIforT     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE cMeanSHIforS     = comlev1, key=ikey_dynamics, kind=isbyte
#ifdef ALLOW_ECCO
CADJ STORE shelficeFreshWaterFlux=comlev1,key=ikey_dynamics,kind=isbyte
CADJ STORE shelficeLoadAnomaly   =comlev1, key=ikey_dynamics,kind=isbyte
#endif /* ALLOW_ECCO */
#endif /* ALLOW_STEEP_ICECAVITY */
