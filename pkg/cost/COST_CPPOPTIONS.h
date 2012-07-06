C $Header: /u/gcmpack/MITgcm/pkg/cost/Attic/COST_CPPOPTIONS.h,v 1.5 2012/07/06 23:12:45 jmc Exp $
C $Name:  $

CBOP
C !ROUTINE: COST_CPPOPTIONS.h
C !INTERFACE:
C #include "COST_CPPOPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for Cost-Function (cost) package:
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifndef COST_OPTIONS_H
#define COST_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_COST
C-- Package-specific options go here
C   Note: most of these options have been shifted to the common header
C         file ECCO_CPPOPTIONS.h

#endif /* ALLOW_COST */
#endif /* COST_OPTIONS_H */
