
CADJ STORE uvel           = comlev1, key = ikey_dynamics
CADJ STORE vvel          = comlev1, key = ikey_dynamics
CADJ STORE salt          = comlev1, key = ikey_dynamics
CADJ STORE totphihyd     = comlev1, key = ikey_dynamics
CADJ STORE pmepr         = comlev1, key = ikey_dynamics
CADJ STORE runoff        = comlev1, key = ikey_dynamics
CADJ STORE surfaceforcingtice = comlev1, key = ikey_dynamics

CADJ STORE area          = comlev1, key = ikey_dynamics
CADJ STORE heff          = comlev1, key = ikey_dynamics
CADJ STORE hsnow         = comlev1, key = ikey_dynamics
CADJ STORE tice          = comlev1, key = ikey_dynamics
# ifdef SEAICE_MULTILEVEL
CADJ STORE tices         = comlev1, key = ikey_dynamics
# endif
# ifdef SEAICE_ALLOW_DYNAMICS
CADJ STORE uice          = comlev1, key = ikey_dynamics
CADJ STORE vice          = comlev1, key = ikey_dynamics
#  ifdef SEAICE_CGRID
CADJ STORE etan          = comlev1, key = ikey_dynamics
CADJ STORE dwatn         = comlev1, key = ikey_dynamics
CADJ STORE seaicemasku,seaicemaskv = comlev1, key = ikey_dynamics
#  endif
# endif
#ifdef ALLOW_COST_ICE
CADJ STORE objf_ice      = comlev1, key = ikey_dynamics
#endif
