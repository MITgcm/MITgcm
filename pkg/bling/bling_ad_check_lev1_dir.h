#ifdef ALLOW_BLING

CADJ STORE pH                = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE fice              = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE atmosP            = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE wind              = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE silica            = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE ak1               = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE ak2               = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE akf               = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE ft                = comlev1, key = ikey_dynamics, kind=isbyte

CADJ STORE irr_mem           = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE chl               = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE poc               = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE phyto_sm          = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE phyto_lg          = comlev1, key = ikey_dynamics, kind=isbyte
# ifndef USE_BLING_V1
CADJ STORE phyto_diaz        = comlev1, key = ikey_dynamics, kind=isbyte
# endif

CADJ STORE dicwind0          = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE dicwind1          = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE atmosp0           = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE atmosp1           = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE silica0           = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE silica1           = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE ice0              = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE ice1              = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE feinput0          = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE feinput1          = comlev1, key = ikey_dynamics, kind=isbyte

#ifdef ALLOW_EXF
CADJ STORE apressure         = comlev1, key = ikey_dynamics, kind=isbyte
CADJ STORE wspeed            = comlev1, key = ikey_dynamics, kind=isbyte
# endif

#endif /* ALLOW_BLING */

