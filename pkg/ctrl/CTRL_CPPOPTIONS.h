C $Header: /u/gcmpack/MITgcm/pkg/ctrl/Attic/CTRL_CPPOPTIONS.h,v 1.3 2011/05/24 17:08:10 jmc Exp $
C $Name:  $

CBOP
C !ROUTINE: CTRL_CPPOPTIONS.h
C !INTERFACE:
C #include "CTRL_CPPOPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for Control (ctrl) package:
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifndef CTRL_OPTIONS_H
#define CTRL_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_CTRL

#include "CPP_OPTIONS.h"

C-- Package-specific options go here
C   Note: most of these options have been shifted to the common header
C         file ECCO_CPPOPTIONS.h

#endif /* ALLOW_CTRL */
#endif /* CTRL_OPTIONS_H */
