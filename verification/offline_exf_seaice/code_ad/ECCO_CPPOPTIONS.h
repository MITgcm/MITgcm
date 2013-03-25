C $Header: /u/gcmpack/MITgcm/verification/offline_exf_seaice/code_ad/ECCO_CPPOPTIONS.h,v 1.6 2013/03/25 02:39:33 gforget Exp $
C $Name:  $

#ifndef ECCO_CPPOPTIONS_H
#define ECCO_CPPOPTIONS_H
#include "AD_CONFIG.h"
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C CPP flags controlling which code is included in the files that
C will be compiled.
C
C ********************************************************************
C ***                         ECCO Package                         ***
C ********************************************************************
C
C       >>> Do a long protocol.
#undef ECCO_VERBOSE
C       >>> use model/src/forward_step.F
#define ALLOW_ECCO_EVOLUTION

C ********************************************************************
C ***                  Adjoint Support Package                     ***
C ********************************************************************

#define ALLOW_AUTODIFF_TAMC
C
C       >>> Checkpointing as handled by TAMC
#define ALLOW_TAMC_CHECKPOINTING
#define AUTODIFF_2_LEVEL_CHECKPOINT
C
C       >>> Extract adjoint state
#define ALLOW_AUTODIFF_MONITOR
C
C o use divided adjoint to split adjoint computations
#undef ALLOW_DIVIDED_ADJOINT
#undef ALLOW_DIVIDED_ADJOINT_MPI

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

C       >>> Cost function contributions
#define ALLOW_ECCO_OLD_FC_PRINT

C       >>> Initial values.
#define ALLOW_THETA0_COST_CONTRIBUTION

C       >>> Atmospheric state and radiation.
#define ALLOW_ATEMP_COST_CONTRIBUTION
#define ALLOW_SWDOWN_COST_CONTRIBUTION

#define ALLOW_COST_ICE

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

#endif /* ECCO_CPPOPTIONS_H */

