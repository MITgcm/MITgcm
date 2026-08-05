#ifdef ALLOW_SHELFICE
# ifndef SHELFICE_OPTIONS_H
      STOP 'need "SHELFICE_OPTIONS.h" to include this header file'
# endif
# ifdef ALLOW_SHELFICE_REMESHING
CADJ STORE kTopC            = comlev1, key=ikey_dynamics
# endif
CADJ STORE shelficeMass     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE shelficeForcingT = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE shelficeForcingS = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE cMeanSHIforT     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE cMeanSHIforS     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE shelficeFreshWaterFlux
CADJ &                      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE shelficeLoadAnomaly
CADJ &                      = comlev1, key=ikey_dynamics, kind=isbyte
#endif
