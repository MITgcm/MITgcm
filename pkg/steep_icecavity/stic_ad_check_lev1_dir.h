CADJ STORE shelficeFreshWaterFlux
CADJ &                      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE shelficeLoadAnomaly
CADJ &                      = comlev1, key=ikey_dynamics, kind=isbyte
#ifdef ALLOW_SHITRANSCOEFF_3D
CADJ STORE shiTransCoeffS3d = comlev1, key=ikey_dynamics, kind=isbyte
# else
CADJ STORE shiTransCoeffS   = comlev1, key=ikey_dynamics, kind=isbyte
#endif
