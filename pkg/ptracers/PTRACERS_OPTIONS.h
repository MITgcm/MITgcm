C $Header: /u/gcmpack/MITgcm/pkg/ptracers/PTRACERS_OPTIONS.h,v 1.4 2003/11/20 15:01:10 jmc Exp $
C $Name:  $

C CPP options file for PTRACERS package
C
C Use this file for selecting options within the PTRACERS package

#ifndef PTRACERS_OPTIONS_H
#define PTRACERS_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_PTRACERS

#include "CPP_OPTIONS.h"

C NUMBER_OF_PTRACERS defines how many passive tracers are allocated/exist.
C This CPP macro is *only* used in PTRACERS.h to set an integer parameter.
C <Please> do not make use of it elsewhere.


C If NUMBER_OF_PTRACERS is not specified elsewhere and ALLOW_PTRACERS
C is set then NUMBER_OF_PTRACERS is set here (default 1)
#ifndef NUMBER_OF_PTRACERS
#define NUMBER_OF_PTRACERS 1
#endif

C CPP Macros go here

#endif /* ALLOW_PTRACERS */
#endif /* PTRACERS_OPTIONS_H */
