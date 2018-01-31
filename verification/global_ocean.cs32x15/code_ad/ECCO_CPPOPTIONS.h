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

C allow use of legacy ecco/ctrl codes
#define ECCO_CTRL_DEPRECATED

C let autodiff_init_varia reset variables to 0
#define ALLOW_AUTODIFF_INIT_OLD

C o Include/exclude code in order to be able to automatically
C   differentiate the MITgcmUV by using the Tangent Linear and
C   Adjoint Model Compiler (TAMC).

#define ALLOW_AUTODIFF_TAMC
C       >>> Checkpointing as handled by TAMC
#define ALLOW_TAMC_CHECKPOINTING

C       >>> Extract adjoint state
#define ALLOW_AUTODIFF_MONITOR

C       >>> DO 2-level checkpointing instead of 3-level
#undef AUTODIFF_2_LEVEL_CHECKPOINT

C o use divided adjoint to split adjoint computations
#undef ALLOW_DIVIDED_ADJOINT

#define ALLOW_AUTODIFF_WHTAPEIO
#define AUTODIFF_USE_MDSFINDUNITS
#define ALLOW_PACKUNPACK_METHOD2
#define AUTODIFF_USE_OLDSTORE_3D
#define AUTODIFF_USE_OLDSTORE_2D

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
#define ALLOW_COST_TEST
#undef ALLOW_COST_TRACER
#undef ALLOW_COST_ATLANTIC_HEAT
#define ALLOW_COST_TSQUARED

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************

#define EXCLUDE_CTRL_PACK
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
C ***             External forcing Package                         ***
C ********************************************************************

C   Bulk formulae related flags.
#define ALLOW_ATM_TEMP
#undef  ALLOW_ATM_WIND
#define ALLOW_DOWNWARD_RADIATION
#ifdef ALLOW_ATM_TEMP
# define ALLOW_BULKFORMULAE
# undef  ALLOW_BULK_LARGEYEAGER04
#endif

C-  Other forcing fields
#define ALLOW_RUNOFF
#undef  ALLOW_RUNOFTEMP
#undef  ALLOW_SALTFLX

C   Use ocean_emissivity*lwdwon in lwFlux. This flag should be define
C   unless to reproduce old results (obtained with inconsistent old code)
#ifdef ALLOW_DOWNWARD_RADIATION
# define EXF_LWDOWN_WITH_EMISSIVITY
#endif

C   Relaxation to monthly climatologies.
#define ALLOW_CLIMSST_RELAXATION
#define ALLOW_CLIMSSS_RELAXATION

C   Use spatial interpolation to interpolate
C   forcing files from input grid to model grid.
#undef USE_EXF_INTERPOLATION

#undef EXF_INTERP_USE_DYNALLOC
#if ( defined (EXF_INTERP_USE_DYNALLOC) & defined (USING_THREADS) )
# define EXF_IREAD_USE_GLOBAL_POINTER
#endif

C >>> No Open boundaries
#undef ALLOW_OBCSN_CONTROL
#undef ALLOW_OBCSS_CONTROL
#undef ALLOW_OBCSW_CONTROL
#undef ALLOW_OBCSE_CONTROL

C ********************************************************************
#endif /* ECCO_CPPOPTIONS_H */
