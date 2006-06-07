C 
C CPP flags controlling which code is included in the files that
C will be compiled.
C
C ********************************************************************
C ***                         ECCO Package                         ***
C ********************************************************************
C
#undef  ALLOW_ECCO_FORWARD_RUN
#undef  ALLOW_ECCO_DIAGNOSTICS_RUN
#undef  ALLOW_ECCO_OPTIMIZATION

C       >>> Do a long protocol.
#undef ECCO_VERBOSE

C ********************************************************************
C ***                  Adjoint Support Package                     ***
C ********************************************************************

C o Include/exclude code in order to be able to automatically
C   differentiate the MITgcmUV by using the Tangent Linear and
C   Adjoint Model Compiler (TAMC).
cph#define INCLUDE_AUTODIFF_PACKAGE
C
#define ALLOW_AUTODIFF_TAMC
C       >>> Checkpointing as handled by TAMC
#define ALLOW_TAMC_CHECKPOINTING
C       Include/exclude adjoint monitor routine
#undef ALLOW_AUTODIFF_MONITOR
C#define ALLOW_AUTODIFF_MONITOR
C       Include/exclude atmosphere-specific code
#undef ALLOW_AUTODIFF_ATMOSPHERE

C ********************************************************************
C ***                     Calender Package                         ***
C ********************************************************************
C
C CPP flags controlling which code is included in the files that
C will be compiled.
C
C o Include the calendar tool.
#undef ALLOW_CALENDAR
#undef ALLOW_CAL_NENDITER

C ********************************************************************
C ***                Cost function Package                         ***
C ********************************************************************
C 
C       >>> Cost function contributions
#define ALLOW_COST
#define ALLOW_COST_TEST
Cml#define ALLOW_COST_TRACER
#undef ALLOW_COST_TRACER
#define ALLOW_COST_DEPTH

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************
C 
#undef  ALLOW_NONDIMENSIONAL_CONTROL_IO
C       >>> Initial values.
Cml#define ALLOW_THETA0_CONTROL
Cml#define ALLOW_SALT0_CONTROL
Cml#define ALLOW_TR10_CONTROL
Cml#define ALLOW_TAUU0_CONTROL
Cml#define ALLOW_TAUV0_CONTROL
Cml#define ALLOW_SFLUX0_CONTROL
Cml#define ALLOW_HFLUX0_CONTROL
#undef ALLOW_SSS0_CONTROL
#undef ALLOW_SST0_CONTROL
Cml#define ALLOW_DIFFKR_CONTROL
#undef ALLOW_KAPGM_CONTROL
C o only topography as a control vector
#define ALLOW_DEPTH_CONTROL 
#ifdef ALLOW_DEPTH_CONTROL
# define ALLOW_CG2D_NSA
# define USE_SMOOTH_MIN
#endif /* ALLOW_DEPTH_CONTROL */

