#ifndef ECCO_CPPOPTIONS_H
#define ECCO_CPPOPTIONS_H
c#include "PACKAGES_CONFIG.h"
c#include "CPP_OPTIONS.h"

C-- Collect here, in a single option-file, options to control which optional
C   features to compile in packages AUTODIFF, COST, CTRL, ECCO, CAL and EXF.
C   If used, this option-file needs to be directly included in CPP_OPTIONS.h
C   Although this method, inherited from ECCO setup, has been traditionally
C   used for all adjoint built, work is in progess to allow to use the
C   standard metod (each of the above pkg get its own options from its
C   specific option-file) also for adjoint built.
C
C ********************************************************************
C ***                         ECCO Package                         ***
C ********************************************************************
C

C allow use of legacy ecco/ctrl codes
#define ECCO_CTRL_DEPRECATED

C       >>> Do a long protocol.
#undef ECCO_VERBOSE

C ********************************************************************
C ***                  Adjoint Support Package                     ***
C ********************************************************************

#define ALLOW_AUTODIFF_TAMC

C       >>> Checkpointing as handled by TAMC
#define ALLOW_TAMC_CHECKPOINTING

C       >>> DO 2-level checkpointing instead of 3-level
#undef AUTODIFF_2_LEVEL_CHECKPOINT

C       >>> Extract adjoint state
#define ALLOW_AUTODIFF_MONITOR

C o use divided adjoint to split adjoint computations
#undef ALLOW_DIVIDED_ADJOINT

c#define ALLOW_AUTODIFF_WHTAPEIO
c#define ALLOW_PACKUNPACK_METHOD2
c#define AUTODIFF_USE_OLDSTORE_2D
c#define AUTODIFF_USE_OLDSTORE_3D
c#define EXCLUDE_WHIO_GLOBUFF_2D
c#define ALLOW_INIT_WHTAPEIO

C ********************************************************************
C ***                Cost function Package                         ***
c ********************************************************************
C

#define ALLOW_THSICE_COST_TEST

C ********************************************************************
C ***               Control vector Package                         ***
C ********************************************************************
C
#define  CTRL_SET_PREC_32
#define  ALLOW_NONDIMENSIONAL_CONTROL_IO

C       >>> Initial values.
#define ALLOW_THETA0_CONTROL

C       >>> Atmospheric state and radiation.
#define  ALLOW_ATEMP_CONTROL
#define  ALLOW_SWDOWN_CONTROL

C ********************************************************************
C ***             External forcing Package                         ***
C ********************************************************************

C   Bulk formulae related flags.
#define  ALLOW_ATM_TEMP
#define  ALLOW_ATM_WIND
#define  ALLOW_DOWNWARD_RADIATION
#define  ALLOW_RUNOFF
#if (defined (ALLOW_ATM_TEMP) || defined (ALLOW_ATM_WIND))
# define ALLOW_BULKFORMULAE
# undef  ALLOW_BULK_LARGEYEAGER04
#endif

C   Zenith Angle/Albedo related flags.
#ifdef ALLOW_DOWNWARD_RADIATION
# undef ALLOW_ZENITHANGLE
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
C   for interpolated vector fields, rotate towards model-grid axis
C   using old rotation formulae (instead of grid-angles)
#undef EXF_USE_OLD_VEC_ROTATION
C   for interpolation around N & S pole, use the old formulation
C   (no pole symmetry, single vector-comp interp, reset to 0 zonal-comp @ N.pole)
#undef EXF_USE_OLD_INTERP_POLE

#define EXF_INTERP_USE_DYNALLOC
#if ( defined (EXF_INTERP_USE_DYNALLOC) && defined (USING_THREADS) )
# define EXF_IREAD_USE_GLOBAL_POINTER
#endif

#endif /* ECCO_CPPOPTIONS_H */
