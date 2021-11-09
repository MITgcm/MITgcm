CBOP
C !ROUTINE: AUTODIFF_OPTIONS.h
C !INTERFACE:
C #include "AUTODIFF_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for AutoDiff (autodiff) package:
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifndef AUTODIFF_OPTIONS_H
#define AUTODIFF_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_AUTODIFF
#ifdef ECCO_CPPOPTIONS_H

C-- When multi-package option-file ECCO_CPPOPTIONS.h is used (directly included
C    in CPP_OPTIONS.h), this option file is left empty since all options that
C   are specific to this package are assumed to be set in ECCO_CPPOPTIONS.h

#else /* ndef ECCO_CPPOPTIONS_H */
C   ==================================================================
C-- Package-specific Options & Macros go here

C o Include/exclude code in order to be able to automatically
C   differentiate the MITgcmUV by using the Tangent Linear and
C   Adjoint Model Compiler (TAMC).
#define ALLOW_AUTODIFF_TAMC

C       >>> Checkpointing as handled by TAMC
#define ALLOW_TAMC_CHECKPOINTING

C       >>> Extract adjoint state
#define ALLOW_AUTODIFF_MONITOR
C       >>> and DYNVARS_DIAG adjoint state
#undef ALLOW_AUTODIFF_MONITOR_DIAG

C       >>> DO 2-level checkpointing instead of 3-level
c#undef AUTODIFF_2_LEVEL_CHECKPOINT

C extend to 4-level checkpointing
c#undef AUTODIFF_4_LEVEL_CHECKPOINT

C o use divided adjoint to split adjoint computations
#undef ALLOW_DIVIDED_ADJOINT

#define ALLOW_AUTODIFF_WHTAPEIO
C Note: comment out the #define below (instead of having an #undef) to
C       enable to set this Option in CPP command line (from the optfile)
c#define AUTODIFF_USE_MDSFINDUNITS
#define ALLOW_PACKUNPACK_METHOD2
#undef AUTODIFF_USE_OLDSTORE_3D
#undef AUTODIFF_USE_OLDSTORE_2D

C o write separate tape files for each ptracer
#undef AUTODIFF_PTRACERS_SPLIT_FILES

C o allow using viscFacInAd to recompute viscosities in AD
#define AUTODIFF_ALLOW_VISCFACADJ

C   ==================================================================
#endif /* ndef ECCO_CPPOPTIONS_H */
#endif /* ALLOW_AUTODIFF */
#endif /* AUTODIFF_OPTIONS_H */
