#ifndef ATM2D_OPTIONS_H
#define ATM2D_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_ATM2D
C     Package-specific Options & Macros go here

C turn on MPI or not
#undef ATM2D_MPI_ON

C- allow single grid-point debugging write to standard-output
#define ALLOW_DBUG_ATM2D

#define JBUGI 89
#define JBUGJ 43

C- undocumented options:
#undef CLM
#undef CLM35
#undef CPL_CHEM
#undef CPL_NEM
#undef CPL_OCEANCO2
#undef CPL_TEM
#undef DATA4TEM
#undef IPCC_EMI
#undef ML_2D
#undef NCEPWIND
#undef OCEAN_3D
#undef ORBITAL_FOR
#undef PREDICTED_AEROSOL

#endif /* ALLOW_ATM2D */
#endif /* ATM2D_OPTIONS_H */
