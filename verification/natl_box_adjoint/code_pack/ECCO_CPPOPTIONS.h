
C 
C CPP flags controlling which code is included in the files that
C will be compiled.
C
C ********************************************************************
C ***                         ECCO Package                         ***
C ********************************************************************
C
C       >>> Important flags for pack/unpack only setup:
#define CTRL_DO_PACK_UNPACK_ONLY
#define  ALLOW_ECCO_OPTIMIZATION
#define ALLOW_DIVIDED_ADJOINT

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

C ********************************************************************
C ***                     Calendar Package                         ***
C ********************************************************************
C 
C CPP flags controlling which code is included in the files that
C will be compiled.

CPH >>>>>> THERE ARE NO MORE CAL OPTIONS TO BE SET <<<<<<

C ********************************************************************
C ***                Cost function Package                         ***
C ********************************************************************
C
#define ALLOW_COST
#ifdef ALLOW_COST
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
# define ALLOW_PRECIP_COST_CONTRIBUTION
# define ALLOW_SWFLUX_COST_CONTRIBUTION
# define ALLOW_UWIND_COST_CONTRIBUTION
# define ALLOW_VWIND_COST_CONTRIBUTION

# define ALLOW_THETA_COST_CONTRIBUTION
# define ALLOW_SALT_COST_CONTRIBUTION
# define ALLOW_SST_COST_CONTRIBUTION
# undef ALLOW_SSS_COST_CONTRIBUTION

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

#endif /* ALLOW_COST */

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************
C 
#define  ALLOW_NONDIMENSIONAL_CONTROL_IO

C       >>> Initial values.
#define ALLOW_THETA0_CONTROL
#define ALLOW_SALT0_CONTROL

C       >>> Surface fluxes.
#undef  ALLOW_HFLUX_CONTROL
#undef  ALLOW_SFLUX_CONTROL
#undef  ALLOW_USTRESS_CONTROL
#undef  ALLOW_VSTRESS_CONTROL

C       >>> Atmospheric state.
#define  ALLOW_ATEMP_CONTROL
#define  ALLOW_AQH_CONTROL
#define  ALLOW_PRECIP_CONTROL
#define  ALLOW_SWFLUX_CONTROL
#define  ALLOW_UWIND_CONTROL
#define  ALLOW_VWIND_CONTROL

C       >>> Radiation
#undef  ALLOW_SWFLUX_CONTROL
#undef  ALLOW_LWFLUX_CONTROL

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
C 
C o Include/exclude the external forcing package. To use this package,
C   you have to include the calendar tool as well. KPP can be switched
C   on or off. The implementation automatically takes care of this.
#define INCLUDE_EXTERNAL_FORCING_PACKAGE

C   Do more printout for the protocol file than usual.
#undef EXF_VERBOSE

C   Bulk formulae related flags.
#define  ALLOW_ATM_TEMP
#define  ALLOW_ATM_WIND
#define ALLOW_BULKFORMULAE

C   Relaxation to monthly climatologies.
#define ALLOW_CLIM_CYCLIC
#undef  ALLOW_CLIMTEMP_RELAXATION
#undef  ALLOW_CLIMSALT_RELAXATION
#undef  ALLOW_CLIMSST_RELAXATION
#undef  ALLOW_CLIMSSS_RELAXATION
