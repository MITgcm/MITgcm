#ifndef ECCO_CPPOPTIONS_H
#define ECCO_CPPOPTIONS_H

C-- Collect here, in a single option-file, options to control which optional
C   features to compile in packages AUTODIFF, COST, CTRL, ECCO, CAL and EXF.
C   If used, this option-file needs to be directly included in CPP_OPTIONS.h
C   Although this method, inherited from ECCO setup, has been traditionally
C   used for all adjoint built, work is in progess to allow to use the
C   standard metod (each of the above pkg get its own options from its
C   specific option-file) also for adjoint built.

C ********************************************************************
C ***                         ECCO Package                         ***
C ********************************************************************

C allow use of legacy ecco/ctrl codes
#define ECCO_CTRL_DEPRECATED

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

#define ALLOW_AUTODIFF_TAMC
C       >>> Checkpointing as handled by TAMC
#define ALLOW_TAMC_CHECKPOINTING
C       Include/exclude adjoint monitor routine
#undef ALLOW_AUTODIFF_MONITOR

C       Include/exclude atmosphere-specific code
#undef ALLOW_AUTODIFF_ATMOSPHERE

C ********************************************************************
C ***                     Calendar Package                         ***
C ********************************************************************

C CPP flags controlling which code is included in the files that
C will be compiled.

C ********************************************************************
C ***                Cost function Package                         ***
C ********************************************************************

C       >>> Cost function contributions
#define ALLOW_COST_TEST
Cml#define ALLOW_COST_TRACER
#undef ALLOW_COST_TRACER
#define ALLOW_COST_DEPTH

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************

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
# define USE_SMOOTH_MIN
#endif /* ALLOW_DEPTH_CONTROL */

C ********************************************************************
#endif /* ECCO_CPPOPTIONS_H */
