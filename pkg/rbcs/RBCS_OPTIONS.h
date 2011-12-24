C $Header: /u/gcmpack/MITgcm/pkg/rbcs/RBCS_OPTIONS.h,v 1.4 2011/12/24 01:09:40 jmc Exp $
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

C use RBCS only in early spin-up phase, reducing
C the relaxation term strength to 0 over 3 years
#undef ALLOW_RBCS_SPIN

#endif /* ALLOW_RBCS */
#endif /* RBCS_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
