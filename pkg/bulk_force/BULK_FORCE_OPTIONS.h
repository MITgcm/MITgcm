#ifndef BULK_FORCE_OPTIONS_H
#define BULK_FORCE_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_BULK_FORCE
C     Package-specific Options & Macros go here

#undef CONSERV_BULKF

C allow to use of AIM surface flux formulation (S/R BULKF_FORMULA_AIM)
C rather than the default (S/R BULKF_FORMULA_LANL)
#define ALLOW_FORMULA_AIM

#endif /* ALLOW_BULK_FORCE */
#endif /* BULK_FORCE_OPTIONS_H */
