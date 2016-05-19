C $Header: /u/gcmpack/MITgcm/pkg/bling/BLING_OPTIONS.h,v 1.1 2016/05/19 20:29:26 mmazloff Exp $
C $Name:  $

#ifndef BLING_OPTIONS_H
#define BLING_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_BLING
C     Package-specific Options & Macros go here

c Prevents negative values in nutrient fields
#define BLING_NO_NEG

c Assume that phytoplankton in the mixed layer experience
c the average light over the mixed layer
c (as in original BLING model)
#define ML_MEAN_LIGHT

c Assume that phytoplankton are homogenized in the mixed layer
#define ML_MEAN_PHYTO

c Determine PAR from shortwave radiation from EXF package
#undef  USE_EXFQSW

c Sub grid scale sediments
#undef  USE_SGS_SED

c Read atmospheric pCO2 values from EXF package
c *** to be specified in EXF_OPTIONS.h ***
c #undef  USE_EXFCO2

c Simplify some parts of the code that are problematic 
c when using the adjoint
#undef  BLING_ADJOINT_SAFE

#endif /* ALLOW_BLING */
#endif /* BLING_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
