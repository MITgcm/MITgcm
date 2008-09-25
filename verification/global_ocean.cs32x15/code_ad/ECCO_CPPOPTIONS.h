C $Header: /u/gcmpack/MITgcm/verification/global_ocean.cs32x15/code_ad/ECCO_CPPOPTIONS.h,v 1.2 2008/09/25 21:45:53 heimbach Exp $
C $Name:  $

C
C CPP flags controlling which code is included in the files that
C will be compiled.

C ********************************************************************
C ***                  Adjoint Support Package                     ***
C ********************************************************************

C o Include/exclude code in order to be able to automatically
C   differentiate the MITgcmUV by using the Tangent Linear and
C   Adjoint Model Compiler (TAMC).
C
#define ALLOW_AUTODIFF_TAMC
C       >>> Checkpointing as handled by TAMC
#define ALLOW_TAMC_CHECKPOINTING
C
C       >>> Extract adjoint state
#define ALLOW_AUTODIFF_MONITOR
C
C       >>> DO 2-level checkpointing instead of 3-level
#undef AUTODIFF_2_LEVEL_CHECKPOINT
C
C o use divided adjoint to split adjoint computations
#undef ALLOW_DIVIDED_ADJOINT
#undef ALLOW_DIVIDED_ADJOINT_MPI

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
#define ALLOW_COST_TSQUARED

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************
C
#define EXCLUDE_CTRL_PACK
#undef  ALLOW_NONDIMENSIONAL_CONTROL_IO
C
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
# undef ALLOW_BULK_LARGEYEAGER04
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

