C $Header: /u/gcmpack/MITgcm/pkg/dic/DIC_OPTIONS.h,v 1.4 2005/08/18 18:24:29 stephd Exp $
C $Name:  $

#ifndef DIC_OPTIONS_H
#define DIC_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_DIC

#include "CPP_OPTIONS.h"

C CPP Macros go here
#undef CAR_DISS
#define DIC_BIOTIC
#undef  ALLOW_FE

#endif /* ALLOW_DIC */
#endif /* DIC_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
