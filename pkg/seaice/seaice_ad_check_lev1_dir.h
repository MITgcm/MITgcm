CADJ STORE hsnow = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE tices = comlev1, key=ikey_dynamics, kind=isbyte

CADJ STORE area  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE heff  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE uice  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vice  = comlev1, key=ikey_dynamics, kind=isbyte

#ifdef AUTODIFF_SOMETIMES_NEEDED
# ifdef EXACT_CONSERV
CADJ STORE pmepr      = comlev1, key = ikey_dynamics
# endif
cphCADJ STORE uvel    = comlev1, key=ikey_dynamics, kind=isbyte
cphCADJ STORE vvel    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE salt    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE totphihyd  = comlev1, key=ikey_dynamics, kind=isbyte
cphCADJ STORE runoff     = comlev1, key=ikey_dynamics, kind=isbyte

CADJ STORE area    = comlev1, key=ikey_dynamics, kind=isbyte
cphCADJ STORE heff    = comlev1, key=ikey_dynamics, kind=isbyte
cphCADJ STORE heffm   = comlev1, key=ikey_dynamics, kind=isbyte
cphCADJ STORE hsnow   = comlev1, key=ikey_dynamics, kind=isbyte
# ifdef SEAICE_ALLOW_DYNAMICS
CADJ STORE uice    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vice    = comlev1, key=ikey_dynamics, kind=isbyte
cphCADJ STORE zeta    = comlev1, key=ikey_dynamics, kind=isbyte
cphCADJ STORE eta     = comlev1, key=ikey_dynamics, kind=isbyte
# endif
#endif /* AUTODIFF_SOMETIMES_NEEDED */

#ifdef SEAICE_CGRID
CADJ STORE stressdivergencex = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE stressdivergencey = comlev1, key=ikey_dynamics, kind=isbyte
#endif
# ifdef SEAICE_ALLOW_DYNAMICS
#  ifdef SEAICE_CGRID
CADJ STORE etan        = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE dwatn       = comlev1, key=ikey_dynamics, kind=isbyte
#ifdef SEAICE_ALLOW_BOTTOMDRAG
CADJ STORE cbotc       = comlev1, key=ikey_dynamics, kind=isbyte
#endif
CADJ STORE seaicemasku = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE seaicemaskv = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
#  ifdef SEAICE_ALLOW_EVP
CADJ STORE seaice_sigma1  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE seaice_sigma2  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE seaice_sigma12 = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
# endif
# ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE hsalt    = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_COST_ICE
CADJ STORE objf_ice    = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_SEAICE_COST_EXPORT
CADJ STORE uHeffExportCell = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vHeffExportCell = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE icevolMeanCell  = comlev1, key=ikey_dynamics, kind=isbyte
# endif

#ifdef ALLOW_OBCS
# ifdef ALLOW_OBCS_EAST
CADJ STORE obeuice,obeuice0,obeuice1 = comlev1,
CADJ &     key=ikey_dynamics, kind=isbyte
CADJ STORE obevice,obevice0,obevice1 = comlev1,
CADJ &     key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_OBCS_NORTH
CADJ STORE obnuice,obnuice0,obnuice1 = comlev1,
CADJ &     key=ikey_dynamics, kind=isbyte
CADJ STORE obnvice,obnvice0,obnvice1 = comlev1,
CADJ &     key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_OBCS_SOUTH
CADJ STORE obsuice,obsuice0,obsuice1 = comlev1,
CADJ &     key=ikey_dynamics, kind=isbyte
CADJ STORE obsvice,obsvice0,obsvice1 = comlev1,
CADJ &     key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_OBCS_WEST
CADJ STORE obwuice,obwuice0,obwuice1 = comlev1,
CADJ &     key=ikey_dynamics, kind=isbyte
CADJ STORE obwvice,obwvice0,obwvice1 = comlev1,
CADJ &     key=ikey_dynamics, kind=isbyte
# endif
#endif

#ifdef ALLOW_SITRACER
CADJ STORE sitracer = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE sitrarea = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE sitrheff = comlev1, key=ikey_dynamics, kind=isbyte
#endif
