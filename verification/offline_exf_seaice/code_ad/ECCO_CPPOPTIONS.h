C $Header: /u/gcmpack/MITgcm/verification/offline_exf_seaice/code_ad/ECCO_CPPOPTIONS.h,v 1.4 2012/08/09 18:15:57 jmc Exp $
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

#undef  ALLOW_ECCO_FORWARD_RUN
#undef  ALLOW_ECCO_DIAGNOSTICS_RUN
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
c
c       >>> Reduce to 2-level checkpointing
#define AUTODIFF_2_LEVEL_CHECKPOINT

C       >>> Extract adjoint state
#undef ALLOW_AUTODIFF_MONITOR

C o use divided adjoint to split adjoint computations
#undef ALLOW_DIVIDED_ADJOINT

C ********************************************************************
C ***                Cost function Package                         ***
C ********************************************************************

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

C   Bulk formulae related flags.
#define  ALLOW_ATM_TEMP
#define  ALLOW_ATM_WIND
#define  ALLOW_DOWNWARD_RADIATION
#define  ALLOW_RUNOFF
#if (defined (ALLOW_ATM_TEMP) || defined (ALLOW_ATM_WIND))
# define ALLOW_BULK_OFFLINE
# define ALLOW_BULKFORMULAE
# undef ALLOW_BULK_LARGEYEAGER04
#endif

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

#define EXF_INTERP_USE_DYNALLOC
#if ( defined (EXF_INTERP_USE_DYNALLOC) && defined (USING_THREADS) )
# define EXF_IREAD_USE_GLOBAL_POINTER
#endif

C ********************************************************************
#endif /* ECCO_CPPOPTIONS_H */
