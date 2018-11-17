C $Header: /u/gcmpack/MITgcm_contrib/gael/verification/global_oce_llc90/code/CTRL_OPTIONS.h,v 1.1 2014/10/20 03:29:00 gforget Exp $
C $Name:  $

CBOP
C !ROUTINE: CTRL_OPTIONS.h
C !INTERFACE:
C #include "CTRL_OPTIONS.h"

C !DESCRIPTION:
C *==================================================================*
C | CPP options file for Control (ctrl) package:
C | Control which optional features to compile in this package code.
C *==================================================================*
CEOP

#ifndef CTRL_OPTIONS_H
#define CTRL_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_CTRL
#ifdef ECCO_CPPOPTIONS_H

C-- When multi-package option-file ECCO_CPPOPTIONS.h is used (directly included
C    in CPP_OPTIONS.h), this option file is left empty since all options that
C   are specific to this package are assumed to be set in ECCO_CPPOPTIONS.h

#else /* ndef ECCO_CPPOPTIONS_H */
C   ==================================================================
C-- Package-specific Options & Macros go here

C o I/O and pack settings
#define CTRL_SET_PREC_32
c the nondim flag below is no longer relevant with gentim[2d,3d] and genarr[2d,3d]
C#undef ALLOW_NONDIMENSIONAL_CONTROL_IO
#define ALLOW_PACKUNPACK_METHOD2

C o sets of controls
#define ALLOW_GENTIM2D_CONTROL
#define ALLOW_GENARR2D_CONTROL
#define ALLOW_GENARR3D_CONTROL

c even with gentim[2d,3d] and genarr[2d,3d],
c flags for bottomdrag, kap[gm,redi], diffkr are still needed
C o bottom drags, genarr2d
#define ALLOW_BOTTOMDRAG_CONTROL
C o kapgm, genarr3d
CAB #define ALLOW_KAPGM_CONTROL
C o kapredi, genarr3d
CAB #define ALLOW_KAPREDI_CONTROL
C o diffkr, genarr3d
CAB #define ALLOW_DIFFKR_CONTROL

C these smooth options below are no longer relevant with gentim[2d,3d] and genarr[2d,3d]
C  o use pkg/smooth correlation operator (incl. smoother) for 3D controls (Weaver, Courtier 01)
C    This CPP option just sets the default for ctrlSmoothCorrel23 to .TRUE.
C#define ALLOW_SMOOTH_CORREL3D
C  o use pkg/smooth correlation operator (incl. smoother) for 2D controls (Weaver, Courtier 01)
C    This CPP option just sets the default for ctrlSmoothCorrel2D to .TRUE.
C#define ALLOW_SMOOTH_CORREL2D

C  o impose bounds on controls
#define ALLOW_ADCTRLBOUND

C   o rotate u/v vector control to zonal/meridional 
C   components
#define ALLOW_ROTATE_UV_CONTROLS

C   ==================================================================
#endif /* ndef ECCO_CPPOPTIONS_H */
#endif /* ALLOW_CTRL */
#endif /* CTRL_OPTIONS_H */

