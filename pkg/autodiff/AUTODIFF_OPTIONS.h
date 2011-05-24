C $Header: /u/gcmpack/MITgcm/pkg/autodiff/AUTODIFF_OPTIONS.h,v 1.4 2011/05/24 17:06:28 jmc Exp $
C $Name:  $

CBOP
C !ROUTINE: AUTODIFF_OPTIONS.h
C !INTERFACE:
C #include "AUTODIFF_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for AUTODIFF package:
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifndef AUTODIFF_OPTIONS_H
#define AUTODIFF_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "AD_CONFIG.h"
#ifdef ALLOW_AUTODIFF

#include "CPP_OPTIONS.h"

C-- Package-specific options go here
C   Note: most of these options have been shifted to the common header
C         file ECCO_CPPOPTIONS.h

#endif /* ALLOW_AUTODIFF */
#endif /* AUTODIFF_OPTIONS_H */
