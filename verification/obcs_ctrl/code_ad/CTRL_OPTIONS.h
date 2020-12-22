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

#undef EXCLUDE_CTRL_PACK
#define ALLOW_NONDIMENSIONAL_CONTROL_IO

C allow use of legacy ecco/ctrl codes
C#define ECCO_CTRL_DEPRECATED
#ifdef ECCO_CTRL_DEPRECATED
C       >>> Initial values.
#define ALLOW_THETA0_CONTROL
#undef ALLOW_SALT0_CONTROL
#undef ALLOW_UVEL0_CONTROL
#undef ALLOW_VVEL0_CONTROL
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
#undef ALLOW_DIFFKR_CONTROL
#undef ALLOW_KAPGM_CONTROL
#undef ALLOW_KAPREDI_CONTROL
#undef ALLOW_BOTTOMDRAG_CONTROL

C       >>> Backward compatibility option (before checkpoint 65p)
#undef ALLOW_KAPGM_CONTROL_OLD
#undef ALLOW_KAPREDI_CONTROL_OLD

#else /* ifndef ECCO_CTRL_DEPRECATED */
C       >>> Generic Control.
C#define ALLOW_GENARR2D_CONTROL
#define ALLOW_GENARR3D_CONTROL
C#define ALLOW_GENTIM2D_CONTROL
#endif /* ECCO_CTRL_DEPRECATED */

C       >>> Open boundaries
c       >>> Make sure that ALLOW_OBCS is defined
#define  ALLOW_OBCSN_CONTROL
#define  ALLOW_OBCSS_CONTROL
#define  ALLOW_OBCSW_CONTROL
#define  ALLOW_OBCSE_CONTROL
#if (defined (ALLOW_OBCSN_CONTROL) || \
     defined (ALLOW_OBCSS_CONTROL) || \
     defined (ALLOW_OBCSW_CONTROL) || \
     defined (ALLOW_OBCSE_CONTROL))
# define ALLOW_OBCS_CONTROL
#endif
#define ALLOW_OBCS_CONTROL_MODES

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