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

C  o  Re-activate deprecated codes in pkg/ecco & pkg/ctrl (but not recommended)
C     and since pkg/ctrl can be used without pkg/ecco, better to have it here
C  o  In this experiment, this option and the corresponding necessary flags
C     to reproduce this experiment are commented by CTRL
CTRL#define ECCO_CTRL_DEPRECATED

#define EXCLUDE_CTRL_PACK
#undef ALLOW_NONDIMENSIONAL_CONTROL_IO

CTRLC       >>> Initial values.
CTRL#define ALLOW_THETA0_CONTROL
CTRL#define ALLOW_SALT0_CONTROL
CTRL#undef ALLOW_UVEL0_CONTROL
CTRL#undef ALLOW_VVEL0_CONTROL
CTRL#undef ALLOW_TR10_CONTROL
CTRL#undef ALLOW_TAUU0_CONTROL
CTRL#undef ALLOW_TAUV0_CONTROL
CTRL#undef ALLOW_SFLUX0_CONTROL
CTRL#undef ALLOW_HFLUX0_CONTROL
CTRL#undef ALLOW_SSS0_CONTROL
CTRL#undef ALLOW_SST0_CONTROL
CTRL
CTRLC       >>> Surface fluxes.
CTRL#undef ALLOW_HFLUX_CONTROL
CTRL#undef ALLOW_SFLUX_CONTROL
CTRL#undef ALLOW_USTRESS_CONTROL
CTRL#undef ALLOW_VSTRESS_CONTROL
CTRL#undef ALLOW_SWFLUX_CONTROL
CTRL#undef ALLOW_LWFLUX_CONTROL
CTRL
CTRLC       >>> Atmospheric state.
CTRL#undef ALLOW_ATEMP_CONTROL
CTRL#undef ALLOW_AQH_CONTROL
CTRL#undef ALLOW_UWIND_CONTROL
CTRL#undef ALLOW_VWIND_CONTROL
CTRL#undef ALLOW_PRECIP_CONTROL
CTRL
CTRLC       >>> Other Control.
CTRL#define ALLOW_DIFFKR_CONTROL
CTRL#undef ALLOW_KAPGM_CONTROL
CTRL#undef ALLOW_KAPREDI_CONTROL
CTRL#undef ALLOW_BOTTOMDRAG_CONTROL
CTRL
CTRLC       >>> Backward compatibility option (before checkpoint 65p)
CTRL#undef ALLOW_KAPGM_CONTROL_OLD
CTRL#undef ALLOW_KAPREDI_CONTROL_OLD
CTRL
CTRLC     >>> pkg/shelfice fluxes.
CTRL#define ALLOW_SHIFWFLX_CONTROL

C       >>> Generic Control.
#undef ALLOW_GENARR2D_CONTROL
#define ALLOW_GENARR3D_CONTROL
#undef ALLOW_GENTIM2D_CONTROL

C  o Rotation of wind/stress controls adjustments
C    from Eastward/Northward to model grid directions
#undef ALLOW_ROTATE_UV_CONTROLS

C  o Originally the first two time-reccords of control
C    variable tau u and tau v were skipped.
C    The CTRL_SKIP_FIRST_TWO_ATM_REC_ALL option extends this
C    to the other the time variable atmospheric controls.
#undef CTRL_SKIP_FIRST_TWO_ATM_REC_ALL

C  o use pkg/smooth correlation operator (incl. smoother) for 2D controls (Weaver, Courtier 01)
C    This CPP option just sets the default for ctrlSmoothCorrel2D to .TRUE.
#undef ALLOW_SMOOTH_CORREL2D
C  o use pkg/smooth correlation operator (incl. smoother) for 3D controls (Weaver, Courtier 01)
C    This CPP option just sets the default for ctrlSmoothCorrel3D to .TRUE.
#undef ALLOW_SMOOTH_CORREL3D

C  o apply pkg/ctrl/ctrl_smooth.F to 2D controls (outside of ctrlSmoothCorrel2D)
#undef ALLOW_CTRL_SMOOTH
C  o apply pkg/smooth/smooth_diff2d.F to 2D controls (outside of ctrlSmoothCorrel2D)
#undef ALLOW_SMOOTH_CTRL2D
C  o apply pkg/smooth/smooth_diff3d.F to 3D controls (outside of ctrlSmoothCorrel3D)
#undef ALLOW_SMOOTH_CTRL3D

C   ==================================================================
#endif /* ndef ECCO_CPPOPTIONS_H */
#endif /* ALLOW_CTRL */
#endif /* CTRL_OPTIONS_H */
