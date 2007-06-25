C $Header: /u/gcmpack/MITgcm/verification/offline_exf_seaice/code_ad/ECCO_CPPOPTIONS.h,v 1.1 2007/06/25 20:15:43 heimbach Exp $
C $Name:  $

C CPP flags controlling which code is included in the files that
C will be compiled.
C
C ********************************************************************
C ***                         ECCO Package                         ***
C ********************************************************************
C
#undef  ALLOW_ECCO_FORWARD_RUN
#undef  ALLOW_ECCO_DIAGNOSTICS_RUN
#define  ALLOW_ECCO_OPTIMIZATION
 
C ********************************************************************
C ***                  Adjoint Support Package                     ***
C ********************************************************************
                                                                                                                        
C o Include/exclude code in order to be able to automatically
C   differentiate the MITgcmUV by using the Tangent Linear and
C   Adjoint Model Compiler (TAMC).
#define INCLUDE_AUTODIFF_PACKAGE
C
#define ALLOW_AUTODIFF_TAMC
C
C       >>> Checkpointing as handled by TAMC
#define ALLOW_TAMC_CHECKPOINTING
c
c       >>> Reduce to 2-level checkpointing
#define AUTODIFF_2_LEVEL_CHECKPOINT
C
C       >>> Extract adjoint state
#define ALLOW_AUTODIFF_MONITOR
C
C o use divided adjoint to split adjoint computations
#undef ALLOW_DIVIDED_ADJOINT

C ********************************************************************
C ***                Cost function Package                         ***
C ********************************************************************
C
# define ALLOW_ATEMP_COST_CONTRIBUTION
# define ALLOW_AQH_COST_CONTRIBUTION
# define ALLOW_UWIND_COST_CONTRIBUTION
# define ALLOW_VWIND_COST_CONTRIBUTION
# define ALLOW_PRECIP_COST_CONTRIBUTION
# define ALLOW_SWDOWN_COST_CONTRIBUTION

cph-test # define ALLOW_SST_COST_CONTRIBUTION

c       >>> Sea-ice volume
# define ALLOW_COST_ICE

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************
C
#define  EXCLUDE_CTRL_PACK

C       >>> Initial values.
#undef ALLOW_THETA0_CONTROL
#undef ALLOW_SALT0_CONTROL

C       >>> Surface fluxes.
#undef  ALLOW_HFLUX_CONTROL
#undef  ALLOW_SFLUX_CONTROL
#undef  ALLOW_USTRESS_CONTROL
#undef  ALLOW_VSTRESS_CONTROL

C       >>> Atmospheric state.
#define  ALLOW_ATEMP_CONTROL
#define  ALLOW_AQH_CONTROL
#define  ALLOW_UWIND_CONTROL
#define  ALLOW_VWIND_CONTROL
#define  ALLOW_PRECIP_CONTROL
#define  ALLOW_SWDOWN_CONTROL

cph-test #define  ALLOW_SST_CONTROL
#undef  ALLOW_SSS_CONTROL

C ********************************************************************
C ***             External forcing Package                         ***
C ********************************************************************
C 

C   Do more printout for the protocol file than usual.
#undef EXF_VERBOSE

C   Bulk formulae related flags.
#define  ALLOW_ATM_TEMP
#define  ALLOW_ATM_WIND
#define  ALLOW_DOWNWARD_RADIATION
#define  ALLOW_RUNOFF
#if (defined (ALLOW_ATM_TEMP) || \
     defined (ALLOW_ATM_WIND))
# define ALLOW_BULK_OFFLINE
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
