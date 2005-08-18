C $Header: /u/gcmpack/MITgcm/verification/dic_example/code/Attic/DIC_OPTIONS.h,v 1.1 2005/08/18 19:31:30 stephd Exp $
C $Name:  $

#ifndef DIC_OPTIONS_H
#define DIC_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_DIC

#include "CPP_OPTIONS.h"

C CPP Macros go here
#define DIC_BIOTIC
#undef  ALLOW_FE
#undef  CAR_DISS

#endif /* ALLOW_DIC */
#endif /* DIC_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
