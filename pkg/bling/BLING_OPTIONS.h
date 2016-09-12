C $Header: /u/gcmpack/MITgcm/pkg/bling/BLING_OPTIONS.h,v 1.3 2016/09/12 22:11:44 mmazloff Exp $
C $Name:  $

#ifndef BLING_OPTIONS_H
#define BLING_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_BLING
C     Package-specific Options & Macros go here

c Active tracer for total phytoplankton biomass
#undef ADVECT_PHYTO

c Prevents negative values in nutrient fields
#define BLING_NO_NEG

c Assume that phytoplankton in the mixed layer experience
c the average light over the mixed layer
c (as in original BLING model)
#undef ML_MEAN_LIGHT

c Assume that phytoplankton are homogenized in the mixed layer
#undef ML_MEAN_PHYTO

c Determine PAR from shortwave radiation from EXF package
#define USE_EXFQSW

c Use local atmospheric pressure from EXF package for fugacity factor
#undef USE_EXF_ATMPRES

c Sub grid scale sediments
#undef  USE_SGS_SED

c Read atmospheric pCO2 values from EXF package
#undef USE_EXFCO2

c In the DVM routine, assume fixed mixed layer depth 
c (so no need to calc MLD in bling_production)
#define FIXED_MLD_DVM

c Simplify some parts of the code that are problematic 
c when using the adjoint
#define BLING_ADJOINT_SAFE

c For adjoint safe, use constant MLD in bling_dvm
#ifdef BLING_ADJOINT_SAFE
#define FIXED_MLD_DVM
#endif

#endif /* ALLOW_BLING */
#endif /* BLING_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
