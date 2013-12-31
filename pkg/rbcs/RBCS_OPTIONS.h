C $Header: /u/gcmpack/MITgcm/pkg/rbcs/RBCS_OPTIONS.h,v 1.5 2013/12/31 22:24:28 jmc Exp $
C $Name:  $

C CPP options file for pkg RBCS
C Use this file for selecting options within package "RBCS"

#ifndef RBCS_OPTIONS_H
#define RBCS_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_RBCS
C Place CPP define/undef flag here

C o disable relaxation conditions on momemtum
#undef DISABLE_RBCS_MOM

#endif /* ALLOW_RBCS */
#endif /* RBCS_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
