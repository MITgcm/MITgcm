
C 
C CPP flags controlling which code is included in the files that
C will be compiled.
C
C ********************************************************************
C ***                         ECCO Package                         ***
C ********************************************************************
C
C o include dump of snap shots for checks
#undef ALLOW_SNAPSHOTS

#define  ALLOW_ECCO_FORWARD_RUN
#undef  ALLOW_ECCO_DIAGNOSTICS_RUN
#undef  ALLOW_ECCO_ADJOINT_RUN
#undef  ALLOW_ECCO_GRADIENT_CHECK
#undef  ALLOW_ECCO_OPTIMIZATION

C       >>> Do a long protocol.
#undef ECCO_VERBOSE

C       >>> Just do a "dry" run ( useful for testing ).
#undef  ALLOW_NO_DYNAMICS
C       >>> Use the Yearly-Monthly-Daily-Stepping call tree.
#undef  ALLOW_YMDS_TREE
C       >>> Do not call stepping
#define ALLOW_STEPPING_CALL

C ********************************************************************
C ***                  Adjoint Support Package                     ***
C ********************************************************************

C o Include/exclude code in order to be able to automatically
C   differentiate the MITgcmUV by using the Tangent Linear and
C   Adjoint Model Compiler (TAMC).
#undef INCLUDE_AUTODIFF_PACKAGE
C
#undef ALLOW_AUTODIFF_TAMC
C       >>> Checkpointing as handled by TAMC
#undef ALLOW_TAMC_CHECKPOINTING

C o use divided adjoint to split adjoint computations
#undef ALLOW_DIVIDED_ADJOINT

C ********************************************************************
C ***                     Calendar Package                         ***
C ********************************************************************
C 
C CPP flags controlling which code is included in the files that
C will be compiled.
C

C o Include the calendar tool.
#define ALLOW_CALENDAR
#define ALLOW_CAL_NENDITER

C ********************************************************************
C ***                Cost function Package                         ***
C ********************************************************************
C 
C       >>> Use the EGM-96 geoid error covariance.
#undef  ALLOW_EGM96_ERROR_COV
#undef  ALLOW_READ_EGM_DATA
C       >>> Use NSCAT data.
#undef  ALLOW_NSCAT_DATA
C       >>> Cost function contributions
#undef ALLOW_HFLUX_COST_CONTRIBUTION
#undef ALLOW_SFLUX_COST_CONTRIBUTION
#undef ALLOW_USTRESS_COST_CONTRIBUTION
#undef ALLOW_VSTRESS_COST_CONTRIBUTION
#undef ALLOW_THETA_COST_CONTRIBUTION
#undef ALLOW_SALT_COST_CONTRIBUTION
#undef ALLOW_SST_COST_CONTRIBUTION
#undef  ALLOW_SSH_COST_CONTRIBUTION
#undef ALLOW_CTDT_COST_CONTRIBUTION
#undef ALLOW_CTDS_COST_CONTRIBUTION
#undef ALLOW_COST_ATLANTIC
C       >>> Projection onto Spherical Harmonics
#undef  ALLOW_SPH_PROJECTION

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************
C 
#undef  ALLOW_NONDIMENSIONAL_CONTROL_IO
C       >>> Replace hooks for the control variables.
#ifdef INCLUDE_ECCO_PACKAGE
#define _GET_HFLUX_CONTROL(a,b,c)   call ctrl_getheatflux(a,b,c)
#define _GET_SFLUX_CONTROL(a,b,c)   call ctrl_getsaltflux(a,b,c)
#define _GET_USTRESS_CONTROL(a,b,c) call ctrl_getzonstress(a,b,c)
#define _GET_VSTRESS_CONTROL(a,b,c) call ctrl_getmerstress(a,b,c)
#define _GET_SWFLUX_CONTROL(a,b,c)
#define _GET_LWFLUX_CONTROL(a,b,c)
#else
#define _GET_HFLUX_CONTROL(a,b,c)
#define _GET_SFLUX_CONTROL(a,b,c)
#define _GET_USTRESS_CONTROL(a,b,c)
#define _GET_VSTRESS_CONTROL(a,b,c)
#define _GET_SWFLUX_CONTROL(a,b,c)
#define _GET_LWFLUX_CONTROL(a,b,c)
#endif

C       >>> Initial values.
#undef ALLOW_THETA0_CONTROL
#undef ALLOW_SALT0_CONTROL

C       >>> Surface fluxes.
#undef ALLOW_HFLUX_CONTROL
#undef ALLOW_SFLUX_CONTROL
#undef ALLOW_USTRESS_CONTROL
#undef ALLOW_VSTRESS_CONTROL
#undef  ALLOW_SWFLUX_CONTROL
#undef  ALLOW_LWFLUX_CONTROL

C       >>> Atmospheric state.
#undef  ALLOW_ATEMP_CONTROL
#undef  ALLOW_AQH_CONTROL
#undef  ALLOW_UWIND_CONTROL
#undef  ALLOW_VWIND_CONTROL
#undef  ALLOW_PRECIP_CONTROL

C ********************************************************************
C ***             External forcing Package                         ***
C ********************************************************************
C 
C o Include/exclude the external forcing package. To use this package,
C   you have to include the calendar tool as well. KPP can be switched
C   on or off. The implementation automatically takes care of this.
#define INCLUDE_EXTERNAL_FORCING_PACKAGE

C   Do more printout for the protocol file than usual.
#define EXF_VERBOSE

C   Bulk formulae related flags.
#undef  ALLOW_BULKFORMULAE
#undef  ALLOW_ATM_TEMP
#undef  ALLOW_ATM_WIND

C   Relaxation to monthly climatologies.
#undef  ALLOW_CLIMTEMP_RELAXATION
#undef  ALLOW_CLIMSALT_RELAXATION
#undef  ALLOW_CLIMSST_RELAXATION
#undef  ALLOW_CLIMSSS_RELAXATION

C   Relaxation to constant surface fields.
#undef  ALLOW_CONST_SST_RELAXATION
#undef  ALLOW_CONST_SSS_RELAXATION
