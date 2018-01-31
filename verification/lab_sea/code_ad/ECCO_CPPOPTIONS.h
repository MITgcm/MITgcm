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
#define ALLOW_DIVIDED_ADJOINT

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

C       >>> Use the EGM-96 geoid error covariance.
# undef  ALLOW_EGM96_ERROR_COV
# undef  ALLOW_READ_EGM_DATA
C       >>> Use NSCAT data.
# undef  ALLOW_NSCAT_DATA
C       >>> Cost function contributions

# undef ALLOW_HFLUX_COST_CONTRIBUTION
# undef ALLOW_SFLUX_COST_CONTRIBUTION
# undef ALLOW_USTRESS_COST_CONTRIBUTION
# undef ALLOW_VSTRESS_COST_CONTRIBUTION

# define ALLOW_ATEMP_COST_CONTRIBUTION
# define ALLOW_AQH_COST_CONTRIBUTION
# define ALLOW_UWIND_COST_CONTRIBUTION
# define ALLOW_VWIND_COST_CONTRIBUTION
# define ALLOW_PRECIP_COST_CONTRIBUTION
# define ALLOW_SNOWPRECIP_COST_CONTRIBUTION
# define ALLOW_SWDOWN_COST_CONTRIBUTION
# define ALLOW_LWDOWN_COST_CONTRIBUTION
# undef ALLOW_EVAP_COST_CONTRIBUTION
# define ALLOW_APRESSURE_COST_CONTRIBUTION
# undef ALLOW_RUNOFF_COST_CONTRIBUTION

# define ALLOW_THETA0_COST_CONTRIBUTION
# define ALLOW_SALT0_COST_CONTRIBUTION
# define ALLOW_THETA_COST_CONTRIBUTION
# define ALLOW_SALT_COST_CONTRIBUTION
# define ALLOW_SST_COST_CONTRIBUTION
# define ALLOW_SSS_COST_CONTRIBUTION

# define ALLOW_SSH_MEAN_COST_CONTRIBUTION
# define ALLOW_SSH_TPANOM_COST_CONTRIBUTION
# define ALLOW_SSH_ERSANOM_COST_CONTRIBUTION
# undef  ALLOW_SPH_PROJECTION
# if (defined (ALLOW_SSH_MEAN_COST_CONTRIBUTION) || \
      defined (ALLOW_SSH_TPANOM_COST_CONTRIBUTION) || \
      defined (ALLOW_SSH_ERSANOM_COST_CONTRIBUTION))
#  define ALLOW_SSH_COST_CONTRIBUTION
# endif

# undef ALLOW_CTDT_COST_CONTRIBUTION
# undef ALLOW_CTDS_COST_CONTRIBUTION
# undef ALLOW_XBT_COST_CONTRIBUTION
# undef ALLOW_COST_ATLANTIC

c       >>> Open boundaries
c       >>> Make sure that ALLOW_OBCS is defined
# undef ALLOW_OBCSN_COST_CONTRIBUTION
# undef ALLOW_OBCSS_COST_CONTRIBUTION
# undef ALLOW_OBCSW_COST_CONTRIBUTION
# undef ALLOW_OBCSE_COST_CONTRIBUTION
# if (defined (ALLOW_OBCSN_COST_CONTRIBUTION) || \
      defined (ALLOW_OBCSS_COST_CONTRIBUTION) || \
      defined (ALLOW_OBCSW_COST_CONTRIBUTION) || \
      defined (ALLOW_OBCSE_COST_CONTRIBUTION))
#  define ALLOW_OBCS_COST_CONTRIBUTION
# endif

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************

#define  ALLOW_NONDIMENSIONAL_CONTROL_IO
#undef  ALLOW_TAMC_SINGLEPREC_COMLEV

C       >>> Initial values.
#define ALLOW_THETA0_CONTROL
#define ALLOW_SALT0_CONTROL

C       >>> Initial seaice
#define ALLOW_SIAREA_CONTROL
#define ALLOW_SIHEFF_CONTROL

C       >>> Surface fluxes.
#undef  ALLOW_HFLUX_CONTROL
#undef  ALLOW_SFLUX_CONTROL
#undef  ALLOW_USTRESS_CONTROL
#undef  ALLOW_VSTRESS_CONTROL

C       >>> Atmospheric state.
#define  ALLOW_ATEMP_CONTROL
#define  ALLOW_AQH_CONTROL
#define  ALLOW_PRECIP_CONTROL
#define  ALLOW_SNOWPRECIP_CONTROL
#define  ALLOW_SWDOWN_CONTROL
#define  ALLOW_LWDOWN_CONTROL
#define  ALLOW_UWIND_CONTROL
#define  ALLOW_VWIND_CONTROL
#undef  ALLOW_EVAP_CONTROL
#define  ALLOW_APRESSURE_CONTROL
#undef  ALLOW_RUNOFF_CONTROL

C       >>> Radiation
#undef  ALLOW_SWFLUX_CONTROL
#undef  ALLOW_LWFLUX_CONTROL

C       >>> seaice init. conditions
#define  ALLOW_SIAREA_CONTROL
#define  ALLOW_SIHEFF_CONTROL
#undef  ALLOW_SIHSNOW_CONTROL

C       >>> relaxation terms
#define  ALLOW_SST_CONTROL
#define  ALLOW_SSS_CONTROL

C       >>> Open boundaries
c       >>> Make sure that ALLOW_OBCS is defined
#undef  ALLOW_OBCSN_CONTROL
#undef  ALLOW_OBCSS_CONTROL
#undef  ALLOW_OBCSW_CONTROL
#undef  ALLOW_OBCSE_CONTROL
#if (defined (ALLOW_OBCSN_CONTROL) || \
     defined (ALLOW_OBCSS_CONTROL) || \
     defined (ALLOW_OBCSW_CONTROL) || \
     defined (ALLOW_OBCSE_CONTROL))
# define ALLOW_OBCS_CONTROL
#endif

C ********************************************************************
C ***             External forcing Package                         ***
C ********************************************************************

C   Bulk formulae related flags.
#undef  ALLOW_BULK_LARGYEAG04
#define  ALLOW_ATM_TEMP
#define  ALLOW_ATM_WIND
#define  ALLOW_BULKFORMULAE
#define  ALLOW_DOWNWARD_RADIATION
#define  ALLOW_RUNOFF
#undef  EXF_READ_EVAP

C   Use ocean_emissivity*lwdwon in lwFlux. This flag should be define
C   unless to reproduce old results (obtained with inconsistent old code)
#ifdef ALLOW_DOWNWARD_RADIATION
# define EXF_LWDOWN_WITH_EMISSIVITY
#endif

C   Relaxation to monthly climatologies.
#define  ALLOW_CLIMSST_RELAXATION
#define  ALLOW_CLIMSSS_RELAXATION

C   Seaice area relaxation code
#define EXF_SEAICE_FRACTION

C ********************************************************************
#endif /* ECCO_CPPOPTIONS_H */
