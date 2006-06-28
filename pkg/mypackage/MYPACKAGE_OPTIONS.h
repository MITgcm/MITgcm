C $Header: /u/gcmpack/MITgcm/pkg/mypackage/MYPACKAGE_OPTIONS.h,v 1.1 2006/06/28 21:26:20 heimbach Exp $
C $Name:  $

C CPP options file for GM/Redi package
C
C Use this file for selecting options within the GM/Redi package

#ifndef MYPACKAGE_OPTIONS_H
#define MYPACKAGE_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_MYPACKAGE

#include "CPP_OPTIONS.h"

C Place CPP define/undef flag here

#undef MYPA_SPECIAL_COMPILE_OPTION1

#define MYPA_SPECIAL_COMPILE_OPTION2

#endif /* ALLOW_MYPACKAGE */
#endif /* MYPACKAGE_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
