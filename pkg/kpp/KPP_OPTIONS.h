C     *==========================================================*
C     | KPP_OPTIONS.h
C     | o CPP options file for KPP package.
C     *==========================================================*
C     | Use this file for selecting options within the KPP
C     | package.
C     *==========================================================*

#ifndef KPP_OPTIONS_H
#define KPP_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_KPP
C     Package-specific Options & Macros go here

C o When set, smooth shear horizontally with 121 filters
#define KPP_SMOOTH_SHSQ
#undef KPP_SMOOTH_DVSQ

C o When set, smooth dbloc KPP variable horizontally
#define KPP_SMOOTH_DBLOC

C o When set, smooth all KPP density variables horizontally
#undef KPP_SMOOTH_DENS
#ifdef KPP_SMOOTH_DENS
#  define KPP_SMOOTH_DBLOC
#endif

C o When set, smooth vertical viscosity horizontally
#undef KPP_SMOOTH_VISC

C o When set, smooth vertical diffusivity horizontally
#undef KPP_SMOOTH_DIFF

C o Get rid of vertical resolution dependence of dVsq term by
C   estimating a surface velocity that is independent of first
C   level thickness in the model.
#undef KPP_ESTIMATE_UREF

C o The default matching of diffusivity as described in Large et al. (1994)
C can be turned off here, which sometimes helps to remove a noise problem
C at the bottom of the mixing layer when interior mixing is noisy. This
C is documented somehow in van Roekel et al. (2018), 10.1029/2018MS001336
C For better backward compatibility, the flags are defined as negative
#undef KPP_DO_NOT_MATCH_DIFFUSIVITIES
#ifndef KPP_DO_NOT_MATCH_DIFFUSIVITIES
C only makes sense if the diffusitivies are matched
# undef KPP_DO_NOT_MATCH_DERIVATIVES
#endif /*  KPP_DO_NOT_MATCH_DIFFUSIVITIES */

C o Include/exclude smooth regularization at the cost of changed results.
C   With this flag defined, some MAX(var,phepsi) are replaced by var+phepsi
#undef KPP_SMOOTH_REGULARISATION

C o reduce shear mxing by shsq**2/(shsq**2+1e-16) according to
C   Polzin (1996), JPO, 1409-1425), so that there will be no shear mixing
C   with very small shear
#undef KPP_SCALE_SHEARMIXING

C o Include/exclude KPP non/local transport terms
#define KPP_GHAT

C o Exclude Interior shear instability mixing
#undef EXCLUDE_KPP_SHEAR_MIX

C o Exclude double diffusive mixing in the interior
#undef EXCLUDE_KPP_DOUBLEDIFF

C o Avoid as many as possible AD recomputations
C   usually not necessary, but useful for testing
#undef KPP_AUTODIFF_EXCESSIVE_STORE

C o Vertically smooth Ri (for interior shear mixing)
#undef ALLOW_KPP_VERTICALLY_SMOOTH

#endif /* ALLOW_KPP */
#endif /* KPP_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
