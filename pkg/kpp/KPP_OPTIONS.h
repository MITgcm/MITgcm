C     /==========================================================\
C     | KPP_OPTIONS.h                                            |
C     | o CPP options file for KPP package.                      |
C     |==========================================================|
C     | Use this file for selecting options within the KPP       |
C     | package.  KPP is enabled with ALLOW_KPP in CPP_OPTIONS.h |
C     \==========================================================/

#include "CPP_OPTIONS.h"

#ifdef ALLOW_KPP

C o Get rid of vertical resolution dependence of dVsq term by
C   estimating a surface velocity that is independent of first
C   level thickness in the model.
#undef KPP_ESTIMATE_UREF

C o When set, test denominators for small values
C   makes code adjoint-friendly by avoiding singularities in
C   the forward code and its derivative
#define KPP_TEST_DENOM

C o When set, use exchange calls rather than recomputation
C   to obtain KPP parameters in overlap region.  This option
C   was added to reduce stack size requirements on the
C   Origin 2000.  It decreases memory and computation
C   requirements at the expense of increased communications.
C   For a 64-processor 360x224x46 MPI configuration on the
C   Exemplar or Origin 2000, wall clock time is about the
C   same whether FRUGAL_KPP is turned on or off.
#undef FRUGAL_KPP

C o When set, smooth zonal shear meridionally and
C   meridional shear zonally with 121 filters
#define KPP_SMOOTH_SHSQ
#undef KPP_SMOOTH_DVSQ

C o When set, smooth dbloc KPP variable horizontally
#define KPP_SMOOTH_DBLOC

C o When set, smooth all KPP density variables horizontally
#undef KPP_SMOOTH_DENS
#ifdef KPP_SMOOTH_DENS
#  define KPP_SMOOTH_DBLOC
#endif

C o As coded KPP_SMOOTH_DBLOC and KPP_SMOOTH_DENS will not work
C   with FRUGAL_KPP switch turned on
#ifdef KPP_SMOOTH_DBLOC
#  undef FRUGAL_KPP
#endif

C o When set, smooth vertical viscosity horizontally
C   Right now this option only works with FRUGAL_KPP turned on
#undef KPP_SMOOTH_VISC
#ifdef KPP_SMOOTH_VISC
#undef KPP_SMOOTH_DBLOC
#undef KPP_SMOOTH_DENS
#define FRUGAL_KPP
#endif

C o When set, smooth vertical diffusivity horizontally
C   Right now this option only works with FRUGAL_KPP turned on
#undef KPP_SMOOTH_DIFF
#ifdef KPP_SMOOTH_DIFF
#undef KPP_SMOOTH_DBLOC
#undef KPP_SMOOTH_DENS
#define FRUGAL_KPP
#endif


C o Include/exclude various time-averaged diagnostic output
C   for saving storage space
#ifdef   INCLUDE_DIAGNOSTICS_INTERFACE_CODE
#define INCLUDE_DIAGNOSTICS_KPP
#undef  INCLUDE_DIAGNOSTICS_KPPDIFFKZSTAVE
#endif

C o Include/exclude KPP non/local transport terms
#define KPP_GHAT

#endif /* ALLOW_KPP */
