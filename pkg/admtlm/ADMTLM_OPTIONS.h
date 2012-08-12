C $Header: /u/gcmpack/MITgcm/pkg/admtlm/ADMTLM_OPTIONS.h,v 1.1 2012/08/12 18:29:25 jmc Exp $
C $Name:  $

CBOP
C !ROUTINE: ADMTLM_OPTIONS.h
C !INTERFACE:
C #include "ADMTLM_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for SVD ? (admtlm) package:
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifndef ADMTLM_OPTIONS_H
#define ADMTLM_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_ADMTLM
#ifdef ECCO_CPPOPTIONS_H

C-- When multi-package option-file ECCO_CPPOPTIONS.h is used (directly included
C    in CPP_OPTIONS.h), this option file is left empty since all options that
C   are specific to this package are assumed to be set in ECCO_CPPOPTIONS.h

#else /* ndef ECCO_CPPOPTIONS_H */

C-- Package-specific Options & Macros go here

#endif /* ndef ECCO_CPPOPTIONS_H */
#endif /* ALLOW_ADMTLM */
#endif /* ADMTLM_OPTIONS_H */
