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

C       >>> Extract adjoint state
#undef ALLOW_AUTODIFF_MONITOR

C o use divided adjoint to split adjoint computations
#undef ALLOW_DIVIDED_ADJOINT

#define ALLOW_AUTODIFF_WHTAPEIO
C o use MDSFINDUINTS instead of AUTODIFF_MDSFINDUNITS to find unique and
C   valid file units, only used when ALLOW_AUTODIFF_WHTAPEIO is defined
C   Note: comment out the #define below (instead of having an #undef) to
C   enable to set this Option in CPP command line (from the optfile)
c#define AUTODIFF_USE_MDSFINDUNITS
C o This is a set of flags that, if defined at the same time, will get
C   rid of the autodiff_store/restore scheme. That is why we define a
C   "macro" flag to set them all at the same time, but they can also be
C   defined individually. This cannot (and should not) be the default,
C   because in some verificaation experiment cases (OpenAd, obcs_ctrl)
C   defining these flags leads TAF to not generate some
C   adexch_xy_rs/adexch_uv_xy_rs routines that are needed in
C   addummy_in_stepping.F
#define AUTODIFF_USE_OLDSTORE
#ifdef AUTODIFF_USE_OLDSTORE
# define AUTODIFF_USE_OLDSTORE_3D
# define AUTODIFF_USE_OLDSTORE_2D
# define AUTODIFF_USE_OLDSTORE_EXF
# define AUTODIFF_USE_OLDSTORE_SEAICE
# define AUTODIFF_USE_OLDSTORE_OBCS
#endif

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
#undef ALLOW_COST_TEST
#undef ALLOW_COST_TRACER
#define ALLOW_COST_ATLANTIC_HEAT

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************

#define ALLOW_GENARR2D_CONTROL
#define ALLOW_GENARR3D_CONTROL

C ********************************************************************
#endif /* ECCO_CPPOPTIONS_H */
