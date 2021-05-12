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

C allow use of legacy ecco/ctrl codes
#define ECCO_CTRL_DEPRECATED

#define EXCLUDE_CTRL_PACK
#undef ALLOW_NONDIMENSIONAL_CONTROL_IO

C       >>> Initial values.
#define ALLOW_THETA0_CONTROL
#define ALLOW_SALT0_CONTROL
#undef ALLOW_TR10_CONTROL
#undef ALLOW_TAUU0_CONTROL
#undef ALLOW_TAUV0_CONTROL
#undef ALLOW_SFLUX0_CONTROL
#undef ALLOW_HFLUX0_CONTROL
#undef ALLOW_SSS0_CONTROL
#undef ALLOW_SST0_CONTROL

C       >>> Surface fluxes.
#undef ALLOW_HFLUX_CONTROL
#undef ALLOW_SFLUX_CONTROL
#undef ALLOW_USTRESS_CONTROL
#undef ALLOW_VSTRESS_CONTROL
#undef ALLOW_SWFLUX_CONTROL
#undef ALLOW_LWFLUX_CONTROL

C       >>> Atmospheric state.
#undef ALLOW_ATEMP_CONTROL
#undef ALLOW_AQH_CONTROL
#undef ALLOW_UWIND_CONTROL
#undef ALLOW_VWIND_CONTROL
#undef ALLOW_PRECIP_CONTROL

C       >>> Other Control.
#define ALLOW_DIFFKR_CONTROL
#undef ALLOW_KAPGM_CONTROL
#undef ALLOW_KAPREDI_CONTROL
#undef ALLOW_BOTTOMDRAG_CONTROL

C       >>> pkg/shelfice fluxes.
#define ALLOW_SHIFWFLX_CONTROL

C       >>> Generic Control.
#undef ALLOW_GENARR2D_CONTROL
#undef ALLOW_GENARR3D_CONTROL
#undef ALLOW_GENTIM2D_CONTROL

C  o Rotation of wind/stress controls adjustments
C    from Eastward/Northward to model grid directions
#undef ALLOW_ROTATE_UV_CONTROLS

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
