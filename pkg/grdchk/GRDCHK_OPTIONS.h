C $Header: /u/gcmpack/MITgcm/pkg/grdchk/GRDCHK_OPTIONS.h,v 1.3 2012/07/06 23:12:45 jmc Exp $
C $Name:  $

CBOP
C !ROUTINE: GRDCHK_OPTIONS.h
C !INTERFACE:
C #include "GRDCHK_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for Gradient-Check (grdchk) package:
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifndef GRDCHK_OPTIONS_H
#define GRDCHK_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_GRDCHK
C-- Package-specific options go here
C   Note: most of these options have been shifted to the common header
C         file ECCO_CPPOPTIONS.h

#endif /* ALLOW_GRDCHK */
#endif /* GRDCHK_OPTIONS_H */
