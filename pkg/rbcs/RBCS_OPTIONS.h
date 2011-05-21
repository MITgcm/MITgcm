C $Header: /u/gcmpack/MITgcm/pkg/rbcs/RBCS_OPTIONS.h,v 1.3 2011/05/21 00:44:53 gforget Exp $
C $Name:  $

C CPP options file for pkg RBCS
C
C Use this file for selecting options within package "RBCS"

#ifndef RBCS_OPTIONS_H
#define RBCS_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_RBCS

#include "CPP_OPTIONS.h"

C Place CPP define/undef flag here

C o disable relaxation conditions on momemtum
#undef DISABLE_RBCS_MOM

C use RBCS only in early spin-up phase, reducing
C the relaxation term strength to 0 over 3 years
#undef ALLOW_RBCS_SPIN

#endif /* ALLOW_RBCS */
#endif /* RBCS_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
