C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_ad_check_lev1_dir.h,v 1.18 2008/05/03 00:02:53 heimbach Exp $
C $Name:  $

#ifdef AUTODIFF_SOMETIMES_NEEDED
# ifdef EXACT_CONSERV
CADJ STORE pmepr         = comlev1, key = ikey_dynamics
# endif
cphCADJ STORE uvel          = comlev1, key = ikey_dynamics
cphCADJ STORE vvel          = comlev1, key = ikey_dynamics
CADJ STORE salt          = comlev1, key = ikey_dynamics
CADJ STORE totphihyd     = comlev1, key = ikey_dynamics
cphCADJ STORE runoff        = comlev1, key = ikey_dynamics

CADJ STORE area          = comlev1, key = ikey_dynamics
cphCADJ STORE heff          = comlev1, key = ikey_dynamics
cphCADJ STORE heffm         = comlev1, key = ikey_dynamics
cphCADJ STORE hsnow         = comlev1, key = ikey_dynamics
cphCADJ STORE tice          = comlev1, key = ikey_dynamics
# ifdef SEAICE_MULTICATEGORY
CADJ STORE tices         = comlev1, key = ikey_dynamics
# endif
# ifdef SEAICE_ALLOW_DYNAMICS
CADJ STORE uice          = comlev1, key = ikey_dynamics
CADJ STORE vice          = comlev1, key = ikey_dynamics
cphCADJ STORE zeta          = comlev1, key = ikey_dynamics
cphCADJ STORE eta           = comlev1, key = ikey_dynamics
# endif
#endif /* AUTODIFF_SOMETIMES_NEEDED */

# ifdef SEAICE_ALLOW_DYNAMICS
#  ifdef SEAICE_CGRID
CADJ STORE etan          = comlev1, key = ikey_dynamics
CADJ STORE dwatn         = comlev1, key = ikey_dynamics
CADJ STORE seaicemasku,seaicemaskv = comlev1, key = ikey_dynamics
#  endif
#  ifdef SEAICE_ALLOW_EVP
CADJ STORE seaice_sigma1  = comlev1, key = ikey_dynamics
CADJ STORE seaice_sigma2  = comlev1, key = ikey_dynamics
CADJ STORE seaice_sigma12 = comlev1, key = ikey_dynamics
#  endif
# endif
# ifdef SEAICE_SALINITY
CADJ STORE hsalt          = comlev1, key = ikey_dynamics
# endif
# ifdef ALLOW_COST_ICE
CADJ STORE objf_ice       = comlev1, key = ikey_dynamics
# endif
# ifdef ALLOW_SEAICE_COST_EXPORT
CADJ STORE uHeffExportCell = comlev1, key = ikey_dynamics
CADJ STORE vHeffExportCell = comlev1, key = ikey_dynamics
# endif
