C $Header: /u/gcmpack/MITgcm/pkg/obcs/OBCS_OPTIONS.h,v 1.3 2003/10/09 04:19:20 edhill Exp $
C $Name:  $
 
C CPP options file for OBCS package
C
C Use this file for selecting options within the OBCS package

#ifndef OBCS_OPTIONS_H
#define OBCS_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_OBCS

#include "CPP_OPTIONS.h"

C This include hooks to the Orlanski Open Boundary Radiation code
#define ALLOW_ORLANSKI

#endif /* ALLOW_OBCS */
#endif /* OBCS_OPTIONS_H */
