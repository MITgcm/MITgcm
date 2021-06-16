C $Header: /u/gcmpack/MITgcm_contrib/ecco_utils/ecco_v4_release3_devel/code/CTRL_OPTIONS.h,v 1.1 2017/05/04 17:46:37 ou.wang Exp $
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
#define ALLOW_NONDIMENSIONAL_CONTROL_IO
#define ALLOW_PACKUNPACK_METHOD2

C This allows for GMREDI controls
#define ALLOW_KAPGM_CONTROL
# undef ALLOW_KAPGM_CONTROL_OLD
#define ALLOW_KAPREDI_CONTROL
# undef ALLOW_KAPREDI_CONTROL_OLD

C This allows for initila UVEL and VVEL controls
#define ALLOW_UVEL0_CONTROL
#define ALLOW_VVEL0_CONTROL

C This allosws updating etaH as well.
#define ALLOW_ETAH_UPDATE

C o sets of controls
#define ALLOW_GENTIM2D_CONTROL
#define ALLOW_GENARR2D_CONTROL
#define ALLOW_GENARR3D_CONTROL

C  o use pkg/smooth correlation operator (incl. smoother) for 3D controls (Weaver, Courtier 01)
C    This CPP option just sets the default for ctrlSmoothCorrel23 to .TRUE.
#define ALLOW_SMOOTH_CORREL3D
C  o use pkg/smooth correlation operator (incl. smoother) for 2D controls (Weaver, Courtier 01)
C    This CPP option just sets the default for ctrlSmoothCorrel2D to .TRUE.
#define ALLOW_SMOOTH_CORREL2D

C  o impose bounds on controls
#define ALLOW_ADCTRLBOUND

C   o rotate u/v vector control to zonal/meridional 
C   components
#define ALLOW_ROTATE_UV_CONTROLS

C   ==================================================================
#endif /* ndef ECCO_CPPOPTIONS_H */
#endif /* ALLOW_CTRL */
#endif /* CTRL_OPTIONS_H */

