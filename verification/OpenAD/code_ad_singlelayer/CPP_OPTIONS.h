C
C CPP flags controlling which code in included in the files that
C will be compiled.

#ifndef CPP_OPTIONS_H
#define CPP_OPTIONS_H

#include "PACKAGES_CONFIG.h"

C o Run as shallow water model
#define SINGLE_LAYER_MODE

#ifdef ALLOW_KPP
#define  SHORTWAVE_HEATING
#endif

C o Include/exclude phi_hyd calculation code
#define INCLUDE_PHIHYD_CALCULATION_CODE

C o Include/exclude call to S/R CONVECT
#undef INCLUDE_CONVECT_CALL

C o Include/exclude call to S/R CALC_DIFFUSIVITY
#undef INCLUDE_CALC_DIFFUSIVITY_CALL

C o Allow nonHydrostatic code
#undef  ALLOW_NONHYDROSTATIC

C o Exact volume conservation
#undef EXACT_CONSERV

C o Execution environment support options
#include "CPP_EEOPTIONS.h"

#ifdef ALLOW_AUTODIFF
# include "ECCO_CPPOPTIONS.h"
#endif

C o Allow full 3D specification of vertical diffusivity
#ifdef ALLOW_DIFFKR_CONTROL
C - Need to be defined if using DIFFKR_CONTROL
C   (alternatively, could have put this in ECCO_CPPOPTIONS)
#define ALLOW_3D_DIFFKR
#else
C - otherwise, can be turned on or off hereafter:
#undef  ALLOW_3D_DIFFKR
#endif /* ALLOW_DIFFKR_CONTROL */

#endif /* CPP_OPTIONS_H */

