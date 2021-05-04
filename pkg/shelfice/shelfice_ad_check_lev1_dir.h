#ifdef ALLOW_SHELFICE
CADJ STORE shelficeMass     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE kTopC            = comlev1, key=ikey_dynamics
CADJ STORE shelficeForcingT = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE shelficeForcingS = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE cMeanSHIforT     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE cMeanSHIforS     = comlev1, key=ikey_dynamics, kind=isbyte
# ifdef ALLOW_SHIFWFLX_CONTROL
CADJ STORE xx_shifwflx0     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE xx_shifwflx1     = comlev1, key=ikey_dynamics, kind=isbyte
# endif
#endif /* ALLOW_SHELFICE */
