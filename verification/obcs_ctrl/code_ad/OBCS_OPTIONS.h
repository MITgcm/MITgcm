C $Header: /u/gcmpack/MITgcm/verification/obcs_ctrl/code_ad/OBCS_OPTIONS.h,v 1.2 2011/12/24 01:17:53 jmc Exp $
C $Name:  $

C CPP options file for OBCS package
C Use this file for selecting options within the OBCS package

#ifndef OBCS_OPTIONS_H
#define OBCS_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_OBCS
C Package-specific Options & Macros go here

C Enable individual open boundaries
#define ALLOW_OBCS_NORTH
#define ALLOW_OBCS_SOUTH
#define ALLOW_OBCS_EAST
#define ALLOW_OBCS_WEST

C This include hooks to the Orlanski Open Boundary Radiation code
#undef ALLOW_ORLANSKI

C Enable OB values to be prescribed via external fields that are read
C from a file
#define ALLOW_OBCS_PRESCRIBE

C Enable OB conditions following Stevens (1990)
#undef ALLOW_OBCS_STEVENS

C This includes hooks to sponge layer treatment of uvel, vvel
#define ALLOW_OBCS_SPONGE

C balance barotropic velocity
#undef ALLOW_OBCS_BALANCE

#endif /* ALLOW_OBCS */
#endif /* OBCS_OPTIONS_H */
