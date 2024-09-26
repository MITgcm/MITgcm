#ifndef CTRL_OPTIONS_H
#define CTRL_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

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
C   This option is only relevant (for pack/unpack) with OBCS_CONTROL:
#undef ALLOW_NONDIMENSIONAL_CONTROL_IO
#define EXCLUDE_CTRL_PACK
#define ALLOW_PACKUNPACK_METHOD2
#undef CTRL_DO_PACK_UNPACK_ONLY
#undef CTRL_PACK_PRECISE
#undef CTRL_UNPACK_PRECISE
#undef CTRL_DELZNORM
#undef ALLOW_CTRL_WETV

C       >>> Other Control.
C   Allows for GMREDI controls
#undef ALLOW_KAPGM_CONTROL
#undef ALLOW_KAPREDI_CONTROL
C   Allows for Vertical Diffusivity controls
#undef ALLOW_DIFFKR_CONTROL
#undef ALLOW_BOTTOMDRAG_CONTROL
#undef ALLOW_DIC_CONTROL

C   Allows bathymetry as a control vector
C   Note: keep this Option separated from generic control since this control
C     involves many new dependencies that we would like to avoid in general.
#undef ALLOW_DEPTH_CONTROL
#ifdef ALLOW_DEPTH_CONTROL
C   Only relevant within DEPTH_CONTROL code:
# define USE_SMOOTH_MIN
#endif /* ALLOW_DEPTH_CONTROL */

C       >>> Generic Control.
#define ALLOW_GENARR2D_CONTROL
#define ALLOW_GENARR3D_CONTROL
#define ALLOW_GENTIM2D_CONTROL
# undef ALLOW_UVEL0_CONTROL
# undef ALLOW_VVEL0_CONTROL
# undef CTRL_SET_OLD_MAXCVARS_30
# undef CTRL_SET_OLD_MAXCVARS_40

C       >>> Open boundaries
#ifdef ALLOW_OBCS
C    Control of Open-Boundaries is meaningless without compiling pkg/obcs
C    Note: Make sure that corresponding OBCS N/S/W/E Option is defined
# define ALLOW_OBCSN_CONTROL
# define ALLOW_OBCSS_CONTROL
# define ALLOW_OBCSW_CONTROL
# define ALLOW_OBCSE_CONTROL
# undef ALLOW_OBCS_CONTROL_MODES
#endif /* ALLOW_OBCS */

C  o Set ALLOW_OBCS_CONTROL (Do not edit/modify):
#if (defined (ALLOW_OBCSN_CONTROL) || \
     defined (ALLOW_OBCSS_CONTROL) || \
     defined (ALLOW_OBCSW_CONTROL) || \
     defined (ALLOW_OBCSE_CONTROL))
# define ALLOW_OBCS_CONTROL
#endif

C  o Impose bounds on controls
#undef ALLOW_ADCTRLBOUND

C  o Rotation of wind/stress controls adjustments
C    from Eastward/Northward to model grid directions
#undef ALLOW_ROTATE_UV_CONTROLS

C  o Originally the first two time-reccords of control
C    variable tau u and tau v were skipped.
C    The CTRL_SKIP_FIRST_TWO_ATM_REC_ALL option extends this
C    to the other the time variable atmospheric controls.
#undef CTRL_SKIP_FIRST_TWO_ATM_REC_ALL

C  Note: this flag turns on extra smoothing code in ctrl_get_gen.F which
C  is inconsistent with the Weaver and Courtier, 2001 algorithm, and
C  should probably not be used. The corresponding 3D flag applied only
C  to deprecated code that is now removed. At some point we will remove
C  this flag and associated code as well.
C  o apply pkg/smooth/smooth_diff2d.F to 2D controls (outside of Smooth_Correl2D)
#undef ALLOW_SMOOTH_CTRL2D

C  o Print more debug info to STDOUT
#undef ALLOW_CTRL_DEBUG

C   ==================================================================
#endif /* ndef ECCO_CPPOPTIONS_H */
#endif /* ALLOW_CTRL */
#endif /* CTRL_OPTIONS_H */
