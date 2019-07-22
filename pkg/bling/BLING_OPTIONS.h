#ifndef BLING_OPTIONS_H
#define BLING_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_BLING
C     Package-specific Options & Macros go here

c BLING+Nitrogen is the default model. It's a version
c of BLING with 8 tracers and 3 phyto classes.
c For the original 6-tracer model of Galbraith et al (2010),
c define USE_BLING_V1 - but note the different order of
c tracers in data.ptracers
#undef USE_BLING_V1

c options for BLING+Nitrogen code:
c SiBLING: add a 9th tracer for silica
#undef USE_SIBLING
c apply remineralization from diel vertical migration
#undef USE_BLING_DVM
c active tracer for total phytoplankton biomass
#undef ADVECT_PHYTO
c sub grid scale sediments - NOT IMPLEMENTED YET
c #undef USE_SGS_SED

c Prevents negative values in nutrient fields
#define BLING_NO_NEG

c Use Liebig function instead of geometric mean of the
c nutrient limitations to calculate maximum phyto growth rate
#define MIN_NUT_LIM

c Assume that phytoplankton in the mixed layer experience
c the average light over the mixed layer
c (as in original BLING model)
#undef ML_MEAN_LIGHT

c Assume that phytoplankton are homogenized in the mixed layer
#define ML_MEAN_PHYTO

c Calculate MLD using a threshold criterion. If undefined,
c MLD is calculated using the second derivative of rho(z)
#undef BLING_USE_THRESHOLD_MLD

c Determine PAR from shortwave radiation Qsw;
c otherwise determined from date and latitude
#define USE_QSW
c use penetrating fraction instead of exponential attenuation
#undef USE_QSW_Z

c Light absorption scheme from Manizza et al. (2005),
c with self shading from phytoplankton
#undef PHYTO_SELF_SHADING

c note: atm pressure from PKG/EXF is always used for air-sea flux
c calculation if available;
c otherwise read from file or set to constant value (1 atm)

c note: winds from PKG/EXF are always used if available;
c otherwise read from file or set to constant value (5 m/s)

c note: ice fraction from PKG/SEAICE or THSICE is always used
c if available;
c otherwise read from file or set to constant value (0)

c note: atm pCO2 from EXF file is always used
c if available;
c otherwise set to constant value in data.bling

c Simplify some parts of the code that are problematic
c when using the adjoint
#define BLING_ADJOINT_SAFE

c For adjoint safe, do not call bling_dvm
#ifdef BLING_ADJOINT_SAFE
#undef USE_BLING_DVM
#endif

C ABIOTIC OPTIONS
C Compile Munhoven (2013)'s "Solvesaphe" package for pH/pCO2
C  can still select Follows et al (2006) solver in data.bling,
C  but will use solvesaphe dissociation coefficient options.
#undef CARBONCHEM_SOLVESAPHE

C In S/R CARBON_CHEM convert ak1 and ak2 to the total pH scale
C  consistent with other coefficients (currently on the seawater scale).
C NOTE: Has NO effect when CARBONCHEM_SOLVESAPHE is defined (different
C  coeffs are used).
#undef CARBONCHEM_TOTALPHSCALE

#endif /* ALLOW_BLING */
#endif /* BLING_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
