C $Header: /u/gcmpack/MITgcm/verification/isomip/code_ad/ECCO_CPPOPTIONS.h,v 1.2 2012/08/09 18:15:57 jmc Exp $
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
C ***                  Adjoint Support Package                     ***
C ********************************************************************

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
#undef ALLOW_DIVIDED_ADJOINT_MPI

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
#define ALLOW_COST_TSQUARED

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************

#define EXCLUDE_CTRL_PACK
#undef  ALLOW_NONDIMENSIONAL_CONTROL_IO

C       >>> Initial values.
#define ALLOW_THETA0_CONTROL
#define ALLOW_SALT0_CONTROL
#define ALLOW_DIFFKR_CONTROL
#define ALLOW_SHIFWFLX_CONTROL
c#define ALLOW_TR10_CONTROL
c#define ALLOW_TAUU0_CONTROL
c#define ALLOW_TAUV0_CONTROL
c#define ALLOW_SFLUX0_CONTROL
c#define ALLOW_HFLUX0_CONTROL
c#undef ALLOW_SSS0_CONTROL
c#undef ALLOW_SST0_CONTROL
c#define ALLOW_DIFFKR_CONTROL

C ********************************************************************
C ***             External forcing Package                         ***
C ********************************************************************

C   Do more printout for the protocol file than usual.
#define EXF_VERBOSE

C   Bulk formulae related flags.
#define  ALLOW_ATM_TEMP
#undef   ALLOW_ATM_WIND
#define  ALLOW_DOWNWARD_RADIATION
#define  ALLOW_RUNOFF
#if (defined (ALLOW_ATM_TEMP) || \
     defined (ALLOW_ATM_WIND))
# define ALLOW_BULKFORMULAE
# undef  ALLOW_BULK_LARGEYEAGER04
#endif

C   Relaxation to monthly climatologies.
#define ALLOW_CLIMSST_RELAXATION
#define ALLOW_CLIMSSS_RELAXATION

C   Use spatial interpolation to interpolate
C   forcing files from input grid to model grid.
#undef USE_EXF_INTERPOLATION

#define EXF_INTERP_USE_DYNALLOC
#if ( defined (EXF_INTERP_USE_DYNALLOC) & defined (USING_THREADS) )
# define EXF_IREAD_USE_GLOBAL_POINTER
#endif

C >>> No Open boundaries
#undef  ALLOW_OBCSN_CONTROL
#undef  ALLOW_OBCSS_CONTROL
#undef  ALLOW_OBCSW_CONTROL
#undef  ALLOW_OBCSE_CONTROL

C ********************************************************************
#endif /* ECCO_CPPOPTIONS_H */
