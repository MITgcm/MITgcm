C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/ECCO_CPPOPTIONS.h,v 1.10 2012/07/06 23:12:45 jmc Exp $
C $Name:  $

#ifndef ECCO_CPPOPTIONS_H
#define ECCO_CPPOPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_ECCO
C CPP flags controlling which code is included in the files that
C will be compiled.

C ********************************************************************
C ***                         ECCO Package                         ***
C ********************************************************************

C o include dump of snap shots for checks
#undef ALLOW_SNAPSHOTS

#define  ALLOW_ECCO_FORWARD_RUN
#undef  ALLOW_ECCO_DIAGNOSTICS_RUN
#undef  ALLOW_ECCO_ADJOINT_RUN
#undef  ALLOW_ECCO_GRADIENT_CHECK
#undef  ALLOW_ECCO_OPTIMIZATION

C Flag to include (dirty adhoc) balancing of boudary fluxes
#undef ALLOW_CTRL_OBCS_BALANCE

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

C       >>> ALLOW_GENCOST_CONTRIBUTION: interactive way to add basic 2D cost function terms.
C       > In data.ecco, this requires the specification of data file (name, frequency,
C         etc.), bar file name for corresp. model average, standard error file name, etc.
C       > In addition, adding such cost terms requires editing ecco_cost.h to increase
C         NGENCOST, and editing cost_gencost_customize.F to implement the actual
C         model average (i.e. the bar file content).
#undef ALLOW_GENCOST_CONTRIBUTION
C       >>> free form version of GENCOST: allows one to use otherwise defined elements (e.g.
C         psbar and and topex data) while taking advantage of the cost function/namelist slots
C         that can be made available using ALLOW_GENCOST_CONTRIBUTION. To this end
C         ALLOW_GENCOST_CONTRIBUTION simply switches off tests that check whether all of the
C         gencost elements (e.g. gencost_barfile and gencost_datafile) are specified in data.ecco.
C       > While this option increases flexibility within the gencost framework, it implies more room
C         for error, so it should be used cautiously, and with good knowledge of the rest of pkg/ecco.
C       > It requires providing a specific cost function routine, and editing cost_gencost_all.F accordingly.
#undef ALLOW_GENCOST_FREEFORM

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

C  o Rotation of wind/stress controls adjustments
C    from Eastward/Northward to model grid directions
#undef ALLOW_ROTATE_UV_CONTROLS

C ********************************************************************
C ***             External forcing Package                         ***
C ********************************************************************
C
C o Include/exclude the external forcing package. To use this package,
C   you have to include the calendar tool as well. KPP can be switched
C   on or off. The implementation automatically takes care of this.
#define ALLOW_EXF

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

#endif /* ALLOW_ECCO */
#endif /* ECCO_CPPOPTIONS_H */
