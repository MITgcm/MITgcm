#ifndef BLING_OPTIONS_H
#define BLING_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_BLING
C     Package-specific Options & Macros go here

C BLING+Nitrogen is the default model. It is a version
C of BLING with 8 tracers and 3 phyto classes.
C For the original 6-tracer model of Galbraith et al (2010),
C define USE_BLING_V1 - but note the different order of tracers in data.ptracers
#undef USE_BLING_V1

C Options for BLING+Nitrogen code:
C SiBLING: add a 9th tracer for silica
#undef USE_SIBLING
C apply remineralization from diel vertical migration
#undef USE_BLING_DVM
C active tracer for total phytoplankton biomass
#undef ADVECT_PHYTO
C sub grid scale sediments - NOT IMPLEMENTED YET
c #undef USE_SGS_SED

C Prevents negative values in nutrient fields
#define BLING_NO_NEG

C Use Liebig function instead of geometric mean of the
C nutrient limitations to calculate maximum phyto growth rate
#define MIN_NUT_LIM

C Allow different phytoplankton groups to have different growth rates and
C nutrient/light limitations. Parameters implemented have yet to be tuned.
#undef SIZE_NUT_LIM

C Assume that phytoplankton in the mixed layer experience
C the average light over the mixed layer (as in original BLING model)
#undef ML_MEAN_LIGHT

C Assume that phytoplankton are homogenized in the mixed layer
#define ML_MEAN_PHYTO

C Calculate MLD using a threshold criterion. If undefined,
C MLD is calculated using the second derivative of rho(z)
#undef BLING_USE_THRESHOLD_MLD

C Determine PAR from shortwave radiation Qsw;
C otherwise determined from date and latitude
#define USE_QSW

C Light absorption scheme from Manizza et al. (2005),
C with self shading from phytoplankton
#undef PHYTO_SELF_SHADING

C Note: atm pressure from PKG/EXF is always used for air-sea flux calculation
C if available; otherwise read from file or set to constant value (1 atm)

C Note: winds from PKG/EXF are always used if available;
C otherwise read from file or set to constant value (5 m/s)

C Note: ice fraction from PKG/SEAICE or THSICE is always used if available;
C otherwise read from file or set to constant value (0)

C Note: atm pCO2 from EXF file is always used if available;
C otherwise set to constant value in data.bling

C Simplify some parts of the code that are problematic when using the adjoint
#define BLING_ADJOINT_SAFE

C For adjoint safe, do not call bling_dvm
#ifdef BLING_ADJOINT_SAFE
#undef USE_BLING_DVM
#endif

C ABIOTIC OPTIONS
C Compile "Solvesaphe" package (Munhoven 2013) for pH/pCO2
C  can still select Follows et al (2006) solver in data.bling,
C  but will use solvesaphe dissociation coefficient options.
#undef CARBONCHEM_SOLVESAPHE

C In S/R CARBON_CHEM convert ak1 and ak2 to the total pH scale
C  consistent with other coefficients (currently on the seawater scale).
C NOTE: Has NO effect when CARBONCHEM_SOLVESAPHE is defined (different
C  coeffs are used).
#undef CARBONCHEM_TOTALPHSCALE

C When calculating the fraction of sinking organic matter, use model biomass diagnostics.
#define NEW_FRAC_EXP

C Assume different nutrient limitations for small and large phytoplankton.
#define SIZE_DEP_LIM

#endif /* ALLOW_BLING */
#endif /* BLING_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
