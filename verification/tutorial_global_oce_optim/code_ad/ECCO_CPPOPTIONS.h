C $Header: /u/gcmpack/MITgcm/verification/tutorial_global_oce_optim/code_ad/ECCO_CPPOPTIONS.h,v 1.2 2012/08/09 18:15:58 jmc Exp $
C $Name:  $

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

#define  ALLOW_ECCO_OPTIMIZATION

C ********************************************************************
C ***                  Adjoint Support Package                     ***
C ********************************************************************

C o Include/exclude code in order to be able to automatically
C   differentiate the MITgcmUV by using the Tangent Linear and
C   Adjoint Model Compiler (TAMC).

#define ALLOW_AUTODIFF_TAMC
C       >>> Checkpointing as handled by TAMC
#define ALLOW_TAMC_CHECKPOINTING

C       >>> Extract adjoint state
#undef ALLOW_AUTODIFF_MONITOR

C o use divided adjoint to split adjoint computations
#undef ALLOW_DIVIDED_ADJOINT

C ********************************************************************
C ***                     Calendar Package                         ***
C ********************************************************************

C CPP flags controlling which code is included in the files that
C will be compiled.

CPH >>>>>> THERE ARE NO MORE CAL OPTIONS TO BE SET <<<<<<

C ********************************************************************
C ***                Cost function Package                         ***
C ********************************************************************

C       >>> Cost function contributions
#define ALLOW_COST_TEMP
#define ALLOW_COST_HFLUXM

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************

#define  ALLOW_NONDIMENSIONAL_CONTROL_IO
C       >>> Initial values.
#undef ALLOW_THETA0_CONTROL
#undef ALLOW_SALT0_CONTROL
#undef ALLOW_TR10_CONTROL
#undef ALLOW_TAUU0_CONTROL
#undef ALLOW_TAUV0_CONTROL
#undef ALLOW_SFLUX0_CONTROL
#undef ALLOW_HFLUX0_CONTROL
#undef ALLOW_SSS0_CONTROL
#undef ALLOW_SST0_CONTROL
#undef ALLOW_DIFFKR_CONTROL
#undef ALLOW_KAPGM_CONTROL
#define ALLOW_HFLUXM_CONTROL

C ********************************************************************
#endif /* ECCO_CPPOPTIONS_H */
