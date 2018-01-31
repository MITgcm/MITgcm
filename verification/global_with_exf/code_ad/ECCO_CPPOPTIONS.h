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

cph >>>>>> !!!!!! SPECIAL SEAICE FLAG FOR TESTING !!!!!! <<<<<<
cph#define  SEAICE_EXCLUDE_FOR_EXACT_AD_TESTING
cph >>>>>> !!!!!! SPECIAL SEAICE FLAG FOR TESTING !!!!!! <<<<<<

cph#define  ALLOW_ECCO_FORWARD_RUN
#define  ALLOW_ECCO_OPTIMIZATION

C       >>> Do a long protocol.
#undef ECCO_VERBOSE

C ********************************************************************
C ***                  Adjoint Support Package                     ***
C ********************************************************************

C o Include/exclude code in order to be able to automatically

#define ALLOW_AUTODIFF_TAMC

C       >>> Checkpointing as handled by TAMC
#define ALLOW_TAMC_CHECKPOINTING

C extend to 4-level checkpointing
#undef AUTODIFF_4_LEVEL_CHECKPOINT

C       >>> Extract adjoint state
#define ALLOW_AUTODIFF_MONITOR

C o use divided adjoint to split adjoint computations
#undef ALLOW_DIVIDED_ADJOINT

C o TAMC compatible subroutine parameter list
#undef AUTODIFF_TAMC_COMPATIBILITY

C ********************************************************************
C ***                     Calendar Package                         ***
C ********************************************************************

C CPP flags controlling which code is included in the files that
C will be compiled.

CPH >>>>>> THERE ARE NO MORE CAL OPTIONS TO BE SET <<<<<<

C ********************************************************************
C ***                Cost function Package                         ***
C ********************************************************************

# define ALLOW_COST_TEST
# define ALLOW_COST_TSQUARED

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************

C       >>> Initial values.
#define ALLOW_THETA0_CONTROL
#define ALLOW_SALT0_CONTROL

C       >>> Surface fluxes.
#define  ALLOW_HFLUX_CONTROL
#define  ALLOW_SFLUX_CONTROL
#define  ALLOW_USTRESS_CONTROL
#define  ALLOW_VSTRESS_CONTROL

C ********************************************************************
C ***             External forcing Package                         ***
C ********************************************************************

C   Relaxation to monthly climatologies.
#define  ALLOW_CLIMSST_RELAXATION
#define  ALLOW_CLIMSSS_RELAXATION

#define USE_EXF_INTERPOLATION
#define EXF_INTERP_USE_DYNALLOC

C ********************************************************************
#endif /* ECCO_CPPOPTIONS_H */
