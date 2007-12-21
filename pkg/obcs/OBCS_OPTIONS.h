C $Header: /u/gcmpack/MITgcm/pkg/obcs/OBCS_OPTIONS.h,v 1.6 2007/12/21 00:39:44 dimitri Exp $
C $Name:  $
 
C CPP options file for OBCS package
C
C Use this file for selecting options within the OBCS package
C
C OBCS is enabled with ALLOW_OBCS in CPP_OPTIONS.h

#ifndef OBCS_OPTIONS_H
#define OBCS_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_OBCS

C Enable individual open boundaries
#define ALLOW_OBCS_NORTH
#define ALLOW_OBCS_SOUTH
#define ALLOW_OBCS_EAST
#define ALLOW_OBCS_WEST

C This include hooks to the Orlanski Open Boundary Radiation code
#define ALLOW_ORLANSKI

C Enable OB values to be prescribed via external fields that are read
C from a file
#undef ALLOW_OBCS_PRESCRIBE

C This includes hooks to sponge layer treatment of uvel, vvel
#undef ALLOW_OBCS_SPONGE

C balance barotropic velocity
#undef ALLOW_OBCS_BALANCE

C     When the prescribed open boundary conditions are incosistent with
C     forcing, for example, six-hourly forcing and monthly sea ice
C     boundary conditions, ice convergence at edges can cause model
C     to blow up.  The following CPP option fixes this problem but this
C     is at the expense of less accurate boundary conditions.
#undef OBCS_SEAICE_AVOID_CONVERGENCE

#endif /* ALLOW_OBCS */
#endif /* OBCS_OPTIONS_H */

