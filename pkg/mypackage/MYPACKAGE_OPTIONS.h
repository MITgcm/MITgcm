C $Header: /u/gcmpack/MITgcm/pkg/mypackage/MYPACKAGE_OPTIONS.h,v 1.4 2011/12/24 01:09:39 jmc Exp $
C $Name:  $

C CPP options file for MYPACKAGE
C Use this file for selecting options within package "MyPackage"

#ifndef MYPACKAGE_OPTIONS_H
#define MYPACKAGE_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_MYPACKAGE
C Place CPP define/undef flag here

C to reduce memory storage, disable unused array with those CPP flags :
#define MYPACKAGE_3D_STATE
#define MYPACKAGE_2D_STATE
#define MYPACKAGE_TENDENCY

#undef MYPA_SPECIAL_COMPILE_OPTION1

#define MYPA_SPECIAL_COMPILE_OPTION2

#endif /* ALLOW_MYPACKAGE */
#endif /* MYPACKAGE_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
