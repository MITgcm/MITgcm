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
C ***                  Adjoint Support Package                     ***
C ********************************************************************

C o Include/exclude code in order to be able to automatically
C   differentiate the MITgcmUV by using the Tangent Linear and
C   Adjoint Model Compiler (TAMC).

#define ALLOW_AUTODIFF_TAMC
C       >>> Checkpointing as handled by TAMC
#define ALLOW_TAMC_CHECKPOINTING
C
C       >>> Extract adjoint state
#undef ALLOW_AUTODIFF_MONITOR
C
C o use divided adjoint to split adjoint computations
#undef ALLOW_DIVIDED_ADJOINT

C ********************************************************************
C ***                     Calender Package                         ***
C ********************************************************************
C
C CPP flags controlling which code is included in the files that
C will be compiled.

CPH >>>>>> THERE ARE NO MORE CAL OPTIONS TO BE SET <<<<<<

C ********************************************************************
C ***                Cost function Package                         ***
C ********************************************************************
C 
C       >>> Cost function contributions
#define ALLOW_COST
#define ALLOW_COST_TEST
#undef ALLOW_COST_TRACER
#undef ALLOW_COST_ATLANTIC_HEAT

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************
C 
#undef  ALLOW_NONDIMENSIONAL_CONTROL_IO
C       >>> Initial values.
#define ALLOW_THETA0_CONTROL
#define ALLOW_SALT0_CONTROL
#define ALLOW_TR10_CONTROL
#define ALLOW_TAUU0_CONTROL
#define ALLOW_TAUV0_CONTROL
#define ALLOW_SFLUX0_CONTROL
#define ALLOW_HFLUX0_CONTROL
#undef ALLOW_SSS0_CONTROL
#undef ALLOW_SST0_CONTROL
#define ALLOW_DIFFKR_CONTROL
#undef ALLOW_KAPGM_CONTROL

C ********************************************************************
#endif /* ECCO_CPPOPTIONS_H */
